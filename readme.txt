
Author: Chad Jarvis (chad@simula.no)

1. Identify paths
   source("identify_paths.R")
   saveRDS(pn, "paths.rds")

2. Cluster and sort
   source("cluster.R")
   inspect path and reiterate until all paths are correct (correct paths can be saved individually in case they fail on the next iteration)
   saveRDS(paths, "paths.rds")

4. Define segments
   adjust the segment size: segment_size = 1.0
   source("define_segments.R")
   saveRDS(pn, "paths_fix.rds")

5. Augment GPS: this step is slow
    paths = readRDS("paths_fix.rds")
    gpsl = readRDS("gpsl.rds")
    source("agument_gps.R")
    saveRDS(gpsi, "gpsi.rds")

6. Merge
   source("merge.R")

7. Apply statistics
   source("apply_stats.R")
