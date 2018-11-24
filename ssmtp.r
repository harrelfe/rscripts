
# See https://help.ubuntu.com/community/EmailAlerts
# https://wiki.archlinux.org/index.php/SSMTP
# https://unix.stackexchange.com/questions/44043
# https://www.nixtutor.com/linux/send-mail-with-gmail-and-ssmtp/
#
# sudo apt-get install ssmtp
# sudo apt-get install sharutils (for uuencode if using attachments)
# Edit ssmtp.conf and revaliases
# sudo cp ssmtp.conf /etc/ssmtp
# sudo cp revaliases /etc/ssmtp
#
# Tests installation:
# echo -e "To: Frank Harrell <harrelfe@gmail.com>\nSubject: Test Subject\n\nBody 1\nBody 2\nBody 3" | ssmtp -vvv harrelfe@gmail.com

ssmtp <- function(to, subject='', body='', attach=NULL, verbose=FALSE) {
    x <- strsplit(to, '<')[[1]]
    toname <- trimws(x[1])
    toaddr <- sub('>', '', trimws(x[2]))
    tf <- tempfile()
    cat('To: ', to, '\n', 
        'Subject: ', subject, '\n\n',
        body, '\n', sep='', file=tf)
    if(length(attach)) for(a in attach)
        system(paste('cat', a, '| uuencode `basename', a, '` >>', tf))
    cmd <- paste('ssmtp', if(verbose) '-vvv', toaddr, '<', tf)
    system(cmd)
    invisible()
}

if(FALSE) {
    body <- 'This is a body of an email message\nLine 2 http://fharrell.com\nLine 3'
    ssmtp('Frank Harrell <harrelfe@gmail.com>', 'Test Subject Line', body)
    ssmtp('Frank Harrell <harrelfe@gmail.com>', 'Test Subject Line', body,
          attach=c('~/tmp/my.docx', 'ssmtp.r'))
}
