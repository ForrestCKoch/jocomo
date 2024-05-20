
get.test.data <- function(){

  .test.data <- coronary.disease.long
  .test.data.pos <- .test.data[which(.test.data$D=='1'),]
  .test.data.neg <- .test.data[which(.test.data$D=='0'),]

  .test.data.pos$fold <- as.numeric(.test.data.pos$sample) %% 4
  .test.data.pos$sample <- as.numeric(.test.data.pos$sample) %/% 4
  .test.data.neg$fold <- as.numeric(.test.data.neg$sample) %% 4
  .test.data.neg$sample <- as.numeric(.test.data.neg$sample) %/% 4 + max(.test.data.pos$sample)

  .test.data <- rbind(.test.data.pos, .test.data.neg)

  #.test.data <- .test.data[sample.int(dim(.test.data)[1]),]
  rownames(.test.data) <- NULL
  colnames(.test.data) <- c("y", "subjects", "models", "x", "folds")
  .test.data[order(.test.data$subjects%%53),]
}
