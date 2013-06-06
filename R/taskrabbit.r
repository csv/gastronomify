#' Find the authenticity_token in the page.
auth.token <- function(text) {
  doc <- htmlParse(text, asText = TRUE)
  xpathApply(doc, '//input[@name="authenticity_token"]', xmlAttrs)[[1]]['value'][[1]]
}

#' Set up the curl hadle.
handle <- function() {
  curl  <- getCurlHandle()
  agent <- 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/27.0.1453.93 Safari/537.36'
  curlSetOpt(cookiejar = "",  useragent = agent, followlocation = TRUE, curl = curl)
}

#' Submit a task to taskrabbit.
curl <- handle()
text <- httpGET('https://www.taskrabbit.com/p/tasks/new', curl = curl)
params <- list(
  utf8 = '✓',
  authenticity_token = auth.token(text),
  after_auth = '/p/tasks/new',
  'user_session[email]' = EMAIL,
  'user_session[password]' = PASSWORD,
  commit = 'Log in'
)
postForm('https://www.taskrabbit.com/user_session', .params = params, curl=curl)
