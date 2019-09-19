# See https://help.ubuntu.com/community/EmailAlerts
# https://wiki.archlinux.org/index.php/SSMTP
# https://unix.stackexchange.com/questions/44043
# https://www.nixtutor.com/linux/send-mail-with-gmail-and-ssmtp/
# https://rich-iannone.github.io/blastula
#
# sudo apt-get install ssmtp
# sudo apt-get install sharutils (for uuencode if using attachments)
# Edit ssmtp.conf and revaliases
# sudo cp ssmtp.conf /etc/ssmtp
# sudo cp revaliases /etc/ssmtp
#
# Tests installation:
# echo -e "To: Frank Harrell <harrelfe@gmail.com>\nSubject: Test Subject\n\nBody 1\nBody 2\nBody 3" | ssmtp -vvv harrelfe@gmail.com
# See also the curl approach:
#  https://www.r-bloggers.com/emayili-sending-email-from-r/

# Send to one recipient if to="email@foo <name>" or Bcc to a list of recipients if to is a vector
# In the latter case todesc is a description to appear after To:
# Note that some smtp servers such as vumc only allow 300 emails to be sent at once.
# Specify batch=300 in this case to send multiple emails to up to batch users
ssmtp <- function(to, subject='', body='',
                  todesc='Unspecified recipients', attach=NULL, verbose=FALSE,
                  logfile=NULL, batch=1e7, dryrun=FALSE) {
  dashes <- '------------------------------------------\n'
  if(length(to) > 1) {
    multusers <- to
    to <- todesc
  } else {
    multusers <- character(0)
    x <- strsplit(to, '<')[[1]]
    toaddr <- sub('>', '', trimws(x[2]))
  }
    tf <- tempfile()
    cat('To: ', to, '\n', 
        'Subject: ', subject, '\n\n',
        body, '\n', sep='', file=tf)
    if(length(attach)) for(a in attach)
        system(paste('cat', a, '| uuencode `basename', a, '` >>', tf))
    if(length(logfile))
       cat(dashes, date(), dashes, file=logfile, sep='', append=TRUE)
    n <- length(multusers)
    if(n > batch) {
      i <- 1
      while(i <= n) {
        j <- min(n, i + batch - 1)
        if(length(logfile)) cat(dashes, 'Sending to user',
          i, 'to', j, '\n', file=logfile, sep='', append=TRUE)
        toad <- paste(multusers[i:j], collapse=',')
        cmd <- paste('ssmtp', if(verbose || length(logfile)) '-vvv', toad, '<', tf,
              if(length(logfile)) paste('>>', logfile, '2>&1'))
        if(dryrun) cat(cmd, '\n') else system(cmd)
        i <- i + batch
      }
    return(invisible())
    }
    toad <- if(length(multusers)) paste(multusers, collapse=',') else toaddr
    cmd <- paste('ssmtp', if(verbose || length(logfile)) '-vvv', toad, '<', tf,
              if(length(logfile)) paste('>>', logfile, '2>&1'))
    if(dryrun) cat(cmd, '\n') else system(cmd)
    invisible()
}

## Send a series of separate emails to a list of recipients, each with their
## own first name greeting.  The body of the email is in a text file.

sendMails <- function(to, names, subject='', filebody,
                      attach=NULL, verbose=FALSE) {
  n <- length(to)
  if(n != length(names)) stop('length mismatch')
  body <- paste0(readLines(filebody), '\n')
  for(i in 1 : n) {
    greet <- paste0('Dear ', names[i], ':')
    cat('Sending to', to[i], ': ', greet, '\n')
    bod <- c(paste0(greet, '\n\n'), body)
    ssmtp(to[i], subject=subject, body=bod, attach=attach, verbose=verbose)
  }
  }

if(FALSE) {
    body <- 'This is a body of an email message\nLine 2 http://fharrell.com\nLine 3\n'
    ssmtp('Frank Harrell <harrelfe@gmail.com>', 'Test Subject Line', body)
    ssmtp('Frank Harrell <harrelfe@gmail.com>', 'Test Subject Line', body,
          attach=c('~/tmp/my.docx', 'ssmtp.r'))
    ssmtp('Frank Harrell <f.harrell@vumc.org>', 'Test to vumc', body,
          attach='~/tmp/my.docx')

    cat(body, file='/tmp/z')
    sendMails(c('Frank Harrell <harrelfe@gmail.com>',
                'Frank Harrell <f.harrell@vumc.org>'),
              c('Frank', 'Frankie'),
              subject='Test Subject Line', filebody='/tmp/z')
}
