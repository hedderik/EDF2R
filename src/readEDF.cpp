
#include <Rcpp.h>
#include "edf.h"
#include "edftypes.h"
#include <string.h>

using namespace Rcpp;

DataFrame parseEDFevents(std::string fname) {

  /* initiation of the EDFFILE struct */
  EDFFILE * ed = NULL;
  int rv;

  /* Command to open the EDF file. The first argument is the file path the EDF file, The second argument indicates whether you
  want consistency checks. The third argument indicates whether you want the events to be loaded/opened as well. The fourth
  argument indicates whether you want the samples to be loaded/opened as well. The fifth argument is a valid pointer to an integer.
  When "rv" is not equal to 0, an error occured. */
  ed = edf_open_file(&fname[0], 0, 1, 0, &rv);

  int sampleCount = 0;

  if (ed != NULL) {
    /* Command to return the number of elements in the opened EDF file. */
    sampleCount = edf_get_element_count(ed);
  }

  // Create the lists which we're going to return as a data.frame:
  NumericVector time(sampleCount);
  StringVector msg(sampleCount);

  // Now, if possible, fill the lists:
  if (ed != NULL) {
    int curEvent = 0;

    /* Initiation of an ALLF_DATA union which consists of the float data later on */
    ALLF_DATA *fd = NULL;

    /* A loop for the total number of events as determined by sampleCount */
    for(int i = 0; i < sampleCount; i++)
    {
      /* Retrieve the type of the next data element in the EDF file */
      int type = edf_get_next_data(ed);

      if (type == MESSAGEEVENT) {
        fd = edf_get_float_data(ed);
        if(!fd->fe.message || fd->fe.message->len <= 0)
        {
          break;
        }

        time[curEvent] = (double)fd->fe.sttime;
        char * c = &(fd->fe.message->c);
        int	l = fd->fe.message->len;
        c[l] = 0;
        msg[curEvent] = c;
        curEvent++;
      }
    }
  }
  Rcpp::DataFrame EDF = Rcpp::DataFrame::create(
    Rcpp::Named("time")=time,
    Rcpp::Named("msg")=msg
  );

  return(EDF);
}



// [[Rcpp::export]]
List parseEDF(std::string fname) {

  /* initiation of the EDFFILE struct */
  EDFFILE * ed = NULL;

  Rcpp::List recInfo = 0;

  int rv;
  int recBlock = 0;

  int n_eye = 3;

  /* Command to open the EDF file. The first argument is the file path the EDF file, The second argument indicates whether you
   want consistency checks. The third argument indicates whether you want the events to be loaded/opened as well. The fourth
   argument indicates whether you want the samples to be loaded/opened as well. The fifth argument is a valid pointer to an integer.
   When "rv" is not equal to 0, an error occured. */
  ed = edf_open_file(&fname[0], 0, 0, 1, &rv);

  if (rv != 0) {
    printf("An error occured when trying to read %s\n", &fname[0]);
    return(0);
  }

  int sampleCount = 0;

  if (ed != NULL) {
    /* Command to return the number of elements in the opened EDF file. */
    sampleCount = edf_get_element_count(ed);
  }

  // Create the lists that we're going to return as a data.frame:
  NumericVector time(sampleCount);
  NumericVector xL(sampleCount);
  NumericVector yL(sampleCount);
  NumericVector dilL(sampleCount);
  NumericVector xR(sampleCount);
  NumericVector yR(sampleCount);
  NumericVector dilR(sampleCount);

  // Now, if possible, fill the lists:
  if (ed != NULL) {
    int curSample = 0;

    /* Initiation of an ALLF_DATA union which consists of the float data later on */
    ALLF_DATA *fd = NULL;

    /* A loop for the total number of events as determined by sampleCount */
    for(int i = 0; i < sampleCount; i++)
    {
      /* Retrieve the type of the next data element in the EDF file */
      int type = edf_get_next_data(ed);

      /* Collect the recording info if this is the first recording info block */
      if (type == RECORDING_INFO) {
        RECORDINGS *curRecInfo;

        curRecInfo = edf_get_recording_data(ed);

        // Only read recording blocks at the start of a recording session
        if (curRecInfo->state == 1) {
          recBlock++;
          // If we find a recording info block, but we have already parsed one,
          // print a message that we'll only report back on the first recording block.
          if (recBlock == 2) {
            printf("Note: found more than one start of a recording block, only parsed first block. See documentation.\n");
          }

          if (recBlock == 1) {

            recInfo = Rcpp::List::create(
              Rcpp::Named("eflags")=curRecInfo->eflags,
              Rcpp::Named("eye")=(int)curRecInfo->eye,
              Rcpp::Named("filterType")=(int)curRecInfo->filter_type,
              Rcpp::Named("posType")=(int)curRecInfo->pos_type,
              Rcpp::Named("pupilType")=(int)curRecInfo->pupil_type,
              Rcpp::Named("recordType")=(int)curRecInfo->record_type,
              Rcpp::Named("recordingMode")=(int)curRecInfo->recording_mode,
              Rcpp::Named("sampleRate")=curRecInfo->sample_rate,
              Rcpp::Named("sflags")=curRecInfo->sflags,
              Rcpp::Named("state")=(int)curRecInfo->state,
              Rcpp::Named("time")=curRecInfo->time
            );

            if (curRecInfo->eye == 3) {
              printf("Collecting information for both eyes\n");
            } else {
              printf("Collecting information for the %s eye\n", (curRecInfo->eye==1?"left":"right"));
            }
            n_eye = curRecInfo->eye;
          }
        }
      }

      /* These will be mostly SAMPLE_TYPE in this function, but not all */
      if(type == SAMPLE_TYPE){
        fd = edf_get_float_data(ed);
        /* In case of a SAMPLE_TYPE, the float data of ed will be the fs (FSAMPLE)
         struct within the ALLF_DATA union. The fs (FSAMPLE) struct contains the data
         of the sample in the EDF file. */
        /* fs.time contains the time of the sample. */
        time[curSample] = (double)fd->fs.time;

        /* if either both eyes or the left eye information is requested: */
        if ( (n_eye == 1) || (n_eye == 3) )
        {
          xL[curSample] = (double)fd->fs.gx[0];
          /* 1.0e+008 is the value when there is no data. In this case
           NaN or NA should be returned */
          if(xL[curSample] == 100000000)
          {
            xL[curSample] = NA_REAL;
          }
          yL[curSample] = (double)fd->fs.gy[0];
          if(yL[curSample] == 100000000)
          {
            yL[curSample] = NA_REAL;
          }
          dilL[curSample] = (double)fd->fs.pa[0];
          /* in case of the pupil dilation this value is 0.0. */
          if(dilL[curSample] == 0)
          {
            dilL[curSample] = NA_REAL;
          }
        } else {
          xL[curSample] = NA_REAL;
          yL[curSample] = NA_REAL;
          dilL[curSample] = NA_REAL;
        }
        /* If either both eyes or the right eye information is requested */
        if ( (n_eye == 2) || (n_eye == 3) )
        {
          xR[curSample] = (double)fd->fs.gx[1];
          if(xR[curSample] == 100000000)
          {
            xR[curSample] = NA_REAL;
          }
          yR[curSample] = (double)fd->fs.gy[1];
          if(yR[curSample] == 100000000)
          {
            yR[curSample] = NA_REAL;
          }
          dilR[curSample] = (double)fd->fs.pa[1];
          if(dilR[curSample] == 0)
          {
            dilR[curSample] = NA_REAL;
          }
        } else {
          xR[curSample] = NA_REAL;
          yR[curSample] = NA_REAL;
          dilR[curSample] = NA_REAL;
        }
        curSample++;
      }
    }
  }

  Rcpp::DataFrame NDF = Rcpp::DataFrame::create(
    Rcpp::Named("time")=time,
    Rcpp::Named("xL")=xL,
    Rcpp::Named("yL")=yL,
    Rcpp::Named("dilL")=dilL,
    Rcpp::Named("xR")=xR,
    Rcpp::Named("yR")=yR,
    Rcpp::Named("dilR")=dilR
  );

  Rcpp::List LL = Rcpp::List::create(
    Rcpp::Named("samples")=NDF,
    Rcpp::Named("recordingInfo")=recInfo,
    Rcpp::Named("events")=parseEDFevents(fname)
  );
  return LL;
}

