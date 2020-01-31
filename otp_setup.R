otp_dir <- "~/Documents/otp"
router <- "georgia"
memory <- 10240

setwd(otp_dir)

if (!"otp.jar" %in% list.files(otp_dir)) {
  opentripplanner::otp_dl_jar(otp_dir)
}

opentripplanner::otp_build_graph("otp.jar", ".", router = router, memory = memory)

opentripplanner::otp_setup("otp.jar", ".", router = router, memory = memory)

otp_con <- opentripplanner::otp_connect()

opentripplanner::otp_plan(otp_con, )
