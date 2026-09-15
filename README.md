# a4a Stock Assessment Training Course

A one-week, modular training course on stock assessment with the
**a4a** (Assessment for All) framework, delivered at the European Commission
Joint Research Centre (JRC), Ispra on 14-18 of September 2026.

The course is built for a small group of experts bringing **their own data**.
Presentation sessions are deliberately short; roughly half of each day is
reserved for participants to apply each step to their own stock.

---

## Attribution and disclaimer

> This course is built upon the work of the developers of the **a4a
> initiative** and of the **FLR project**. The teaching material here
> re-organises and re-presents their methods, software and documentation for
> a specific training format. All credit for the underlying framework,
> and for the scientific and software work it rests on, belongs to them.
>
> Any errors introduced in adapting that work for this course are ours alone.

### The a4a initiative

The framework is described in:

> Jardim, E., Millar, C. P., Mosqueira, I., Scott, F., Osio, G. C.,
> Ferretti, M., Alzorriz, N., and Orio, A. 2015. *What if stock assessment
> is as simple as a linear model? The a4a initiative.*
> ICES Journal of Marine Science, 72(1): 232–236.
> <https://doi.org/10.1093/icesjms/fsu050>

Authors of *Age-structured stock assessment modeling with R — The a4a
Initiative*, the reference text for this course:

| Author | Affiliation |
|:--|:--|
| **Ernesto Jardim** | Instituto Português do Mar e Atmosfera (IPMA), Lisbon, Portugal · National Institute of Aquatic Resources, Technical University of Denmark (DTU-Aqua), Copenhagen, Denmark |
| **Colin Millar** | International Council for the Exploration of the Sea (ICES), Copenhagen, Denmark |
| **Iago Mosqueira** | Wageningen Marine Research (WMR), IJmuiden, The Netherlands |

<!-- TODO add contact e-mail addresses if you want them public -->

### The FLR project

a4a is built on **FLR** (Fisheries Library for R). The framework is described in:

> Kell, L. T., Mosqueira, I., Grosjean, P., Fromentin, J-M., Garcia, D.,
> Hillary, R., Jardim, E., Mardle, S., Pastoors, M. A., Poos, J. J.,
> Scott, F., and Scott, R. D. 2007. *FLR: an open-source framework for the
> evaluation and development of management strategies.*
> ICES Journal of Marine Science, 64(4): 640–646.

FLR project: <https://flr-project.org>

### Teaching material

Parts of the Day 1 material are adapted from FLR training tutorials
originally written by **Iago Mosqueira** (WMR) and subsequently modified by
JRC colleagues, and from a4a training courses previously delivered at the JRC.

The rest of the days are also based on two previous courses delivered at JRC, 
one in the Summer of 2024 supported also by GFCM and one in May of 2025.

<!-- TODO confirm this wording with the original authors before making the
     repository public -->

---

## Course structure

Four pillars over five days:

| Pillar | Topic | Days |
|:--|:--|:--|
| 1 | Data preparation & input | Day 1 |
| 2 | Stock assessment workflow | Days 2–3 |
| 3 | Advanced diagnostics & retrospective | Day 4 |
| 4 | Reporting & presenting | Day 5 |

Every module is **self-contained**, so sessions can be skipped, reordered or
expanded depending on the level and needs of the participants.

### Day 1 — Data preparation and input

| Module | Title |
|:--|:--|
| M1.0 | Welcome and course overview |
| M1.1 | Introduction to FLR |
| M1.2 | `FLQuant` — the building block |
| M1.3 | `FLStock` — structure, slots and meaning |
| M1.4 | `FLIndex` — survey and CPUE data |
| M1.5 | Building your own `FLStock` and `FLIndices` |
| M1.6 | Inspecting, manipulating and visualising |

---

## Repository layout

```
course/
  _assets/        shared logo, beamer preamble, theme
  _data/          demo datasets used by the hands-on scripts
  _templates/     module template (Quarto → beamer PDF)
  day01/ ... day05/
    MN.N_name/
      MN.N_name.qmd      presentation source
      MN.N_name.pdf      presentation, ready to hand out
      handson_*.R        guided script, run together in the session
      exercise_*.R       own-data template, where applicable
```

`sandbox/` holds the original source material the course was adapted from.
It is **not** included in this repository.

---

## Requirements

R 4.1 or later, and:

```r
install.packages(
  c("FLCore", "FLa4a", "ggplotFL", "a4adiags", "FLBRP", "FLasher"),
  repos = "https://flr.r-universe.dev"
)
```

Participants can verify their setup by running
`course/day01/M1.0_welcome/handson_M1.0_setup_check.R`, which loads every
package, fits a small model and prints a PASS/FAIL report.

### Rebuilding the presentations

The slides are written in Quarto and render to standalone PDF via XeLaTeX,
using the `metropolis` beamer theme:

```bash
quarto render course/day01/M1.0_welcome/M1.0_welcome.qmd
```

---

## Licence

<!-- TODO choose and confirm. The earlier JRC/FLR courses used:
       presentations — Creative Commons Attribution-ShareAlike
       code          — GPL-3.0
       a4a code      — European Union Public Licence (EUPL) v1.2
     Check what applies before making this repository public. -->

---

## Contact

European Commission, Joint Research Centre (JRC), Ispra, Italy.

<!-- TODO course contact address -->
