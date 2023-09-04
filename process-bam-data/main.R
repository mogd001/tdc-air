##### Process BAM 5028i (Beta Attenuation Mass/Monitor) Data from Plunket site
#
# Matt Ogden, September 2022
#
##################

# This script merges datafiles downloaded from the BAM 5028i Richmond Central at Plunket, preparing it for upload to hilltop.
# Download the data and save to the directory as shown below.

# Processing directory (where .dat files are located)
#directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(024) 5028i Download 02-08-2022"
#directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(025) 5028i Download 15-09-2022"
#directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(027) 5028i Download 16-05-2023"
directory <- "M:/Datafiles/Downloads&OtherSources/AIRQUALITY/Richmond/Plunket/5028i BAM/(028) 5028i Download 04-09-2023"

write_comments_to_database <- FALSE

source("2_clean_bam_data.R")

print("completed")