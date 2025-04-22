install.packages('rsconnect')



rsconnect::setAccountInfo(name='twoot',
                          token='6694C214433989BFC223891E04CCEDF0',
                          secret='<SECRET>')
rsconnect::setAccountInfo(name='twoot',
                          token='6694C214433989BFC223891E04CCEDF0',
                          secret='r7/riun4EXqMRlvUpTFVrXD9o5NLFGg57GoO6j3z')

library(rsconnect)
rsconnect::deployApp('final_project/app.R')
