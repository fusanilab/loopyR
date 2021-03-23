
#' Download Results Data
#'
#' @description
#'
#' @param
#' @param
#' @param
#'
#' @return
#'


download_tracking_data <-
    function(asset_id, group_key) {
        # This function uses the main loopy_api function, but the call might be different enough to warrant a whole new function.
        url <- sprintf("/api/1.0/result/download/asset/%s/", asset_id)

        response <- loopy_api(
            url = url,
            asset_id = asset_id,
            group_key = group_key,
            format = "csv"
        )

        if(response$status_code == 202){
            # Need to add in a check and wait.
            try <- 0
            time <- 1
            n_tries <- 5
            while(try < n_tries){
                job_status <- check_job_status(response$content$job_id)
                if(job_status == 200){
                    break
                }else{
                    Sys.sleep(time = time)
                    time <- time*2
                    try <- try + 1
                }
            }
        }

        if(try == 5){
            warning("Request timed out: Try call again later")
        }else{
            # Make the call again
            response <- loopy_api(
                url = url,
                asset_id = asset_id,
                group_key = group_key,
                format = "csv"
            )

        }

        return(response$content)
    }



#' Check job status
#'
#' @description
#'
#' @param
#'
#' @return
#'
#'
check_job_status <-
    function(job_id){
        #
        url <- sprintf("/api/1.0/job/%s/status", job_id)

        response <- loopy_api(url, verbose = '1')

        if(response$content$status == "finished"){
            message("Job is finished: Data has been prepared/cached")
            out <- response$response
        }else if(response$response == 202){
            message("Data is being prepared")
            out <- response$response
        }else{
            ## I have to do more testing to see what the range of responses are.
            warning("Data preparation may have failed.")
            out <- response$response
        }
        return(out)
    }

