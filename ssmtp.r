
## See https://help.ubuntu.com/community/EmailAlerts
# Also see https://wiki.archlinux.org/index.php/SSMTP
# sudo apt-get install ssmtp
# sudo cp ssmtp.conf /etc/ssmtp/ (contact harrelfe@gmail.com for examples)
# sudo cp revaliases /etc/ssmtp/ ( " " " )
# Tests installation:
# echo -e "To: Frank Harrell <harrelfe@gmail.com>\nSubject: Test Subject\n\nBody 1\nBody 2\nBody 3" | ssmtp -vvv harrelfe@gmail.com

ssmtp <- function(to, subject, body, verbose=FALSE) {
    x <- strsplit(to, '<')[[1]]
    toname <- trimws(x[1])
    toaddr <- sub('>', '', trimws(x[2]))
    tf <- tempfile()
    cat('To: ', to, '\n', file=tf, sep='')
    cat('Subject: ', subject, '\n\n', sep='', file=tf, append=TRUE)
    cat(body, '\n', sep='', file=tf, append=TRUE)
    cmd <- paste('ssmtp', if(verbose) '-vvv', toaddr, '<', tf)
    system(cmd)
    invisible()
}

body <- 'This is a body of an email message\nLine 2\nLine 3'
# ssmtp('Frank Harrell <harrelfe@gmail.com>', 'Test Subject Line', body)

