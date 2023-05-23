##### Process BAM (Beta Attenuation Mass/Monitor) Data #####
#
# Matt Ogden, September 2022
#
##################

# TO COMPLETE DOCUMENTATION
#
#
#

# Processing directory (where .dat files are located)
#directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(024) 5028i Download 02-08-2022"
#directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(025) 5028i Download 15-09-2022"
directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(027) 5028i Download 16-05-2023"

write_comments_to_database <- FALSE

source("2_clean_bam_data.R")

print("completed")