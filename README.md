# Statistical Learning Test (SLT)

The SLT package implements the Statistical Learning Test, a musical statistical-learning task in which participants classify short melodies generated from artificial grammars. Melodies are presented in pairs of "styles" (grammatical vs. non-grammatical/scrambled), and participants learn to tell them apart over the course of the test.

The package currently supports four stimulus versions, selected via the `version` argument to `SLT()` / `SLT_standalone()`:

- **version 1** — original Markov-chain grammar, fixed block design (`SLT_item_bank`)
- **version 2** — Markov-chain grammar, gamified with composer-name pairs and Easy/Medium/Hard difficulty blocks (`SLT_item_bank2`)
- **version 3** — deterministic Reber finite-state grammar (`SLT_item_bank3`, `grammar_type == "reber_fsg"`, 36 items)
- **version 4** — deterministic Loui finite-state grammar on a Bohlen-Pierce scale (`SLT_item_bank3`, `grammar_type == "loui_fsg"`, 50 items)

Versions 3 and 4 record per-trial reaction times (`rt_decision_ms`, `rt_total_ms`) in addition to accuracy.


## Citation

We also advise mentioning the software versions you used,
in particular the versions of the `SLT` and `psychTestR` packages.
You can find these version numbers from R by running the following commands:

``` r
library(SLT)
library(psychTestR)
if (!require(devtools)) install.packages("devtools")
x <- devtools::session_info()
x$packages[x$packages$package %in% c("SLT", "psychTestR"), ]
```

## Installation instructions (local use)

1. If you don't have R installed, install it from here: https://cloud.r-project.org/

2. Open R.

3. Install the ‘devtools’ package with the following command:

`install.packages('devtools')`

4. Install the SLT:

`devtools::install_github('klausfrieler/SLT')`

## Usage

### Quick demo 

You can demo the SLT at the R console, as follows:

``` r
# Load the SLT package
library(SLT)

# Run a demo test, with feedback as you progress through the test,
# and not saving your data
SLT_demo()

# Run a demo test, skipping the training phase, and only asking 5 questions, as well a changing the language
SLT_demo(num_items = 5, language = "en")
```

### Testing a participant

The `SLT_standalone()` function is designed for real data collection.
By default, feedback is disabled (`with_feedback = FALSE`). Use the `version` argument to select the stimulus set (1–4, see above).

``` r
# Load the SLT package
library(SLT)

# Run the test as if for a participant, using default settings,
# saving data, and with a custom admin password
SLT_standalone(admin_password = "put-your-password-here", version = 3)
```

You will need to enter a participant ID for each participant.
This will be stored along with their results.

Each time you test a new participant,
rerun the `SLT_standalone()` function,
and a new participation session will begin.

You can retrieve your data by starting up a participation session,
entering the admin panel using your admin password,
and downloading your data.
For more details on the psychTestR interface, 
see http://psychtestr.com/.

The SLT currently supports English (en), German (de), Russian (ru), and Nederlands (nl).
You can select one of these languages by passing a language code as 
an argument to `SLT_standalone()`, e.g., `SLT_standalone(languages = "de")`,
or alternatively by passing it as a URL parameter to the test browser,
eg. http://127.0.0.1:4412/?language=DE (note that the `p_id` argument must be empty).

## Installation instructions (Shiny Server)

1. Complete the installation instructions described under 'Local use'.
2. If not already installed, install Shiny Server Open Source:
https://www.rstudio.com/products/shiny/download-server/
3. Navigate to the Shiny Server app directory.

`cd /srv/shiny-server`

4. Make a folder to contain your new Shiny app.
The name of this folder will correspond to the URL.

`sudo mkdir SLT`

5. Make a text file in this folder called `app.R`
specifying the R code to run the app.

- To open the text editor: `sudo nano SLT/app.R`
- Write the following in the text file:

``` r
library(SLT)
SLT_standalone(admin_password = "put-your-password-here")
```

- Save the file (CTRL-O).

6. Change the permissions of your app directory so that `psychTestR`
can write its temporary files there.

`sudo chown -R shiny SLT`

where `shiny` is the username for the Shiny process user
(this is the usual default).

7. Navigate to your new shiny app, with a URL that looks like this:
`http://my-web-page.org:3838/SLT

## Implementation notes

Versions 1 and 2 present items in a fixed block design (with composer-name pairs per block in v2). Versions 3 and 4 present the entire item pool for a single grammar type (Reber or Loui) per session, in randomized order (with a constraint that the first few trials contain both styles), and additionally record per-trial reaction times.

## Usage notes

- The SLT runs in your web browser.
- By default, image files are hosted online on our servers.
The test therefore requires internet connectivity.
