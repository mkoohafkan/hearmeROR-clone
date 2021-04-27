#' Alert From a Template
#'
#' Construct the contents of an email/text alert from a YAML
#' template.
#'
#' @param varlist a named list of variables that are to
#'   inserted into the email content. Variable names must
#'   exactly match the placeholders in the YAML template.
#'   Required variables are `Station`, `Datetime`, `Stage`,
#'   and `Threshold`.
#' @param template.file The YAML template for the alert.
#' @param figure A `ggplot` object.
#' @return A named list.
#'
#' @importFrom glue glue_data
#' @importFrom base64enc dataURI
#' @importFrom ggplot2 ggsave
#' @keywords internal
alert_from_template = function(varlist, template.file, figure = NULL) {
  template = use_template(template.file)
  template$Subject = glue_data(varlist, template$Subject)[[1]]
  template$Body = glue_data(varlist, template$Body)[[1]]
  template$Figure = figure
  template
}

#' Send Email Alert
#'
#' Send an email alert.
#'
#' @param Template a named list, i.e. output of `alert_from_template()`.
#' @param use_ssl, verbose additional arguments to `curl::send_mail()`.
#'
#' @importFrom stringr str_split
#' @importFrom glue glue
#' @importFrom emayili server envelope from to subject text attachment
#' @keywords internal
send_alert = function(template, use_ssl = FALSE, verbose = FALSE) {
  note = paste("This is an automated warning.",
    "Please do not reply to this message.")

  # server
  smtp = server(host = "mailhost.water.ca.gov", reuse = TRUE,
    insecure = !use_ssl)
  # email
  if (length(template$Recipients) > 0L) {
    msg = envelope()
    msg = from(msg, template$Sender)
    msg = to(msg, template$Recipients)
    msg = subject(msg, template$Subject)
    msg = text(msg, paste(template$Body, note, sep = "\n\n"))
    if (!is.null(template$Figure)) {
      tf = tempfile(fileext = ".png")
      ggsave(tf, template$Figure, width = 5, height = 5)
      msg = attachment(msg, tf)
    }
    response = smtp(msg, verbose = verbose)
    if (response$status_code != 250L) {
      warning(glue("Email may not have succeeded, SMTP server ",
        "returned status code {response$status_code}"))
    }
  }
  # send text
  if (length(template$PhoneRecipients) > 0L) {
    for (Recipient in template$PhoneRecipients) {
      msg = envelope()
      msg = from(msg, template$Sender)
      msg = to(msg, Recipient)
      msg = text(msg, paste(template$Body, note, sep = "\n\n"))
      response = smtp(msg, verbose = verbose)
      if (response$status_code != 250L) {
        warning(glue("Text message may not have succeeded, SMTP ",
          "server returned status code {response$status_code}"))
      }
    }
  }
  invisible(TRUE)
}

#' Use Alert Template
#'
#' Use the specified alert template for messaging.
#'
#' @param template.path The path to the template YAML file.
#'
#' @importFrom yaml read_yaml
#' @export
use_template = function(template.path) {
  if (!file.exists(template.path))
    stop("Could not find file specified")
  this.template = read_yaml(template.path)
  template.fields = names(this.template)
  if (!("Host" %in% template.fields))
    stop("Template does not contain field \"Host\".")
  if (!any(c("Recipients", "PhoneRecipients") %in% template.fields))
    stop("Template does not contain one of fields: ",
      "\"Recipients\", \"PhoneRecipients\".")
  if (!any(c("Subject", "Body") %in% template.fields))
    stop("Template does not contain one of fields: ",
      "\"Subject\", \"Body\".")
  this.template
}
