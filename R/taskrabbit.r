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
  
  # Log in
  params <- list(
    utf8 = '✓',
    authenticity_token = auth.token(text),
    after_auth = '/p/tasks/new',
    'user_session[email]' = EMAIL,
    'user_session[password]' = PASSWORD,
    commit = 'Log in'
  )
  text <- postForm('https://www.taskrabbit.com/user_session', .params = params, curl=curl)
  
  # Post task
  params <- list(
    'utf8' = '✓',
    'authenticity_token' = auth.token(text),
    'task[id]' = '',
    'task[name]' = 'Not really a task',
    'task[category_id]' = '999',
    'task[start_end]' = 'start_at',
    'extra[datepicker]' = 'Friday, November 8',
    'task[date]' = 'Friday, November 8',
    'task[datetime]' = 'Fri Nov 08 2013 00:00:00 GMT-0800 (PST)',
    'task[patron_flow]' = 'general',
    'task[time]' = '',
    'task[locations_attributes][0][freeform_address]' = 'Mission Dolores Park, San Francisco, CA, United States',
    'task[locations_attributes][0][lat]' = '37.79165709999999',
    'task[locations_attributes][0][lng]' = '-122.394419',
    'task[locations_attributes][0][parent_id]' = '',
    'task[locations_attributes][0][id]' = '',
    'task[review_runners]' = 'false',
    'task[named_price]' = '1',
    'task[description]' = 'Make me a tasty sandwich.'
  )
  text <- postForm('https://www.taskrabbit.com/p/tasks', .params = params, curl=curl)
