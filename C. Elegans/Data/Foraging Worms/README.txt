Foraging data
=============

This is the "foraging" dataset from:

   Broekmans, Onno, Rodgers, Jarlath, Ryu, William, and Stephens, Greg,
   Resolving coiled shapes reveals new reorientation behaviors in C. elegans.

Corresponding author:

   Greg J. Stephens <g.j.stephens@vu.nl>


About the data
--------------

Data were produced in the lab of William S. Ryu by Matthew Bueno de Mesquita,
and were first analyzed for [1]. See there for a more detailed description of
the data collection and experimental conditions.

Data is included for 12 individuals, each an N2 L4-stage Caenorhabditis elegans.
Worms 7 and 12 were not analyzed in the 2011 PNAS paper above. Recordings took
place for 35 minutes at a 32 Hz framerate (67,200 frames per worm in total). The
recordings started approximately 5 min after each worm was removed from a
bacterial lawn (food). The (ventral/dorsal) omega turn orientations as
reanalyzed on 2011.08.10 is (L R L R R R ? L R L R ?). Be careful, as it appears
that worms 7 and 12 involve lots of crossed shapes and turning.


Archive contents
----------------

This archive contains one subdirectory per worm, for 12 worms in total. In each
subdirectory, the following files can be found:

 - O*.jpg

   Video frames, taken at a 32 Hz framerate. The image pixel resolution is
   405 pixels/mm. 67,200 frames per worm.

 - data.txt

   A text file with the following columns:

      1. Timestamp (in seconds);
      2. X position (in stepper units);
      3. Y position (in stepper units);
      4. (ignore)

   Stepper units can be converted into millimeter by multiplying with 1/788.
   Note that position values are only updated every 4 frames.

 - tracking.txt

   Our tracking results for each frame, as a tab-separated text file. Note that,
   as described in the paper, we have tracked movies that were downsampled to 16
   Hz by skipping every other frame, hence each file contains only 33,600 rows.
   Each column is an eigenmode value, with a1 in the first column, a2 in the
   second, etc. To reconstitute full centerlines, you will also need the
   eigenworms; see the 'eigenworms.txt' file below.


Other files in this archive are:

 - eigenworms.txt

   Tab-separated text file containing a matrix with the eigenworms from the
   original 'eigenworms' paper [2]. Each column is an eigenworm, with one
   backbone tangent angle per row.


References
----------

 1. Stephens, G.J., Bueno de Mesquita, M., Ryu, W.S., & Bialek, W. (2011).
    Emergence of long timescales and stereotyped behaviors in Caenorhabditis
    elegans. Proceedings of the National Academy of Sciences of the United
    States of America, 108(18), 7286–7289. doi:10.1073/pnas.1007868108

 2. Stephens et al., Dimensionality and dynamics in the behavior of C. elegans,
    PLoS Computational Biology, e1000028 (2008).
    doi:10.1371/journal.pcbi.1000028

