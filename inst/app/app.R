# gsm.bio's app, as a server such as Posit Connect runs it.
#
# This file is the whole deployment: put it in a folder of its own and deploy
# the folder (the article "The app, and putting it on Posit Connect" on
# gsm.bio's site has the call). As it stands it opens on the synthetic study
# that ships with gsm.bio, and a reader loads a study of their own in the
# app's Data view.
#
# To open on a study's own tables, read them here and hand them to RunApp():
#
#   dfResults <- haven::read_xpt("results.xpt")
#   dfParticipants <- haven::read_xpt("participants.xpt")
#   RunApp(as.data.frame(dfResults), as.data.frame(dfParticipants))
#
# The tables are read under gsm.bio's column names (USUBJID, TEST, STRESN,
# VISIT and VISITNUM in the results): rename the columns first.
#
# The three packages are attached by name so that the server's record of what
# the app needs lists each of them: shiny runs it, haven reads a .xpt or a
# .sas7bdat file a reader loads, and gsm.bio is the app.
library(shiny)
library(haven)
library(gsm.bio)

RunApp()
