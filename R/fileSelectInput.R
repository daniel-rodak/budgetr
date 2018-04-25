#' Choose a File Interactively
#'
#' Display a file selection dialog
#'
#' @param default which folder to show initially (ignored on Windows)
#' @param caption the caption on the selection dialog (ignored on Windows)
#'
#' @details Uses an Apple Script/\link{choose.file} to display a file
#'   selection dialog. With \code{default = NA}, the initial file selection is
#'   determined by default behavior of the "choose file" system command.
#'   Otherwise, paths are expanded with \link{path.expand}.
#'
#' @return A length one character vector, character NA if 'Cancel' was selected.
#' @export
choose.file = function(default = NA, caption = NA) {
  if (Sys.info()['sysname'] == 'Darwin') {
    command = 'osascript'
    args = '-e "POSIX path of (choose file{{prompt}}{{default}})"'

    if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
      prompt = sprintf(' with prompt \\"%s\\"', caption)
    } else {
      prompt = ''
    }
    args = sub('{{prompt}}', prompt, args, fixed = T)

    if (!is.null(default) && !is.na(default) && nzchar(default)) {
      default = sprintf(' default location \\"%s\\"', path.expand(default))
    } else {
      default = ''
    }
    args = sub('{{default}}', default, args, fixed = T)

    suppressWarnings({
      path = system2(command, args = args, stderr = TRUE)
    })
    if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
      # user canceled
      path = NA
    }
  } else if (Sys.info()['sysname'] == 'Windows') {
      path <- try(file.choose())
      if (inherits(path, 'try-error')) {
        path <- NA
      }
  } else {
    path <- NA
  }
  return(path)
}

#' File Selection Control
#'
#' Create a file selection control to select a file on the server
#'
#' @param inputId The \code{input} slot that will be used to access the value
#' @param label Display label for the control, or NULL for no label
#' @param value Initial value.  Paths are exapnded via
#'   \code{\link{path.expand}}.
#'
#' @details This widget relies on \link{choose.file}/ to present an
#'   interactive dialog to users for selecting a file on the local filesystem.
#'   Therefore, this widget is intended for shiny apps that are run locally -
#'   i.e. on the same system that files/directories are to be accessed - and not
#'   from hosted applications (e.g. from shinyapps.io).
#'
#' @return A file input control that can be added to a UI definition.
#'
#' @seealso \link{updateFileSelectInput}, \link{readFileSelectInput},
#'   \link{choose.file}, \link{choose.filename}
#' @export
fileSelectInput = function(inputId, label, value = NULL) {
  if (!is.null(value) && !is.na(value)) {
    value = path.expand(value)
  }

  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(src = 'js/directory_input_binding.js')
      )
    ),

    shiny::div(
      class = 'form-group directory-input-container',
      shiny:::`%AND%`(label, shiny::tags$label(label)),
      shiny::div(
        shiny::span(
          class = 'col-xs-9 col-md-11',
          style = 'padding-left: 0; padding-right: 5px;',
          shiny::div(
            class = 'input-group shiny-input-container',
            style = 'width:100%;',
            shiny::div(class = 'input-group-addon', shiny::icon('file')),
            shiny::tags$input(
              id = sprintf('%s__chosen_dir', inputId),
              value = value,
              type = 'text',
              class = 'form-control directory-input-chosen-dir',
              readonly = 'readonly'
            )
          )
        ),
        shiny::span(
          class = 'shiny-input-container',
          shiny::tags$button(
            id = inputId,
            class = 'btn btn-default directory-input',
            '...'
          )
        )
      )
    )

  )

}

#' Change the value of a fileSelectInput on the client
#'
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#' @param inputId The id of the input object.
#' @param value A directory path to set
#' @param ... Additional arguments passed to \link{choose.file}. Only
#'   used if \code{value} is \code{NULL}.
#'
#' @details Sends a message to the client, telling it to change the value of the
#' input object.  For \code{fileSelectInput} objects, this changes the value
#' displayed in the text-field and triggers a client-side change event.  A
#' directory selection dialog is not displayed.
#' @export
updateFileSelectInput = function(session, inputId, value = NULL, ...) {
  if (is.null(value)) {
    value = choose.file(...)
  }
  session$sendInputMessage(inputId, list(chosen_dir = value))
}

#' Read the value of a fileSelectInput
#'
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#' @param inputId The id of the input object
#'
#' @details Reads the value of the text field associated with a
#' \code{fileSelectInput} object that stores the user selected directory path.
#' @export
readFileSelectInput = function(session, inputId) {
  session$input[[sprintf('%s__chosen_dir', inputId)]]
}



#' Choose a File Name Interactively
#'
#' Display a file name selection dialog
#'
#' @param default which folder to show initially (ignored on Windows)
#' @param caption the caption on the selection dialog (ignored on Windows)
#'
#' @details Uses an Apple Script/\link{choose.filename} to display a file
#'   selection dialog. With \code{default = NA}, the initial file selection is
#'   determined by default behavior of the "choose file" system command.
#'   Otherwise, paths are expanded with \link{path.expand}.
#'
#' @return A length one character vector, character NA if 'Cancel' was selected.
#' @export
choose.filename = function(default = NA, caption = NA) {
  if (Sys.info()['sysname'] == 'Darwin') {
    command = 'osascript'
    args = '-e "POSIX path of (choose file name{{prompt}}{{default}})"'

    if (!is.null(caption) && !is.na(caption) && nzchar(caption)) {
      prompt = sprintf(' with prompt \\"%s\\"', caption)
    } else {
      prompt = ''
    }
    args = sub('{{prompt}}', prompt, args, fixed = T)

    if (!is.null(default) && !is.na(default) && nzchar(default)) {
      default = sprintf(' default location \\"%s\\"', path.expand(default))
    } else {
      default = ''
    }
    args = sub('{{default}}', default, args, fixed = T)

    suppressWarnings({
      path = system2(command, args = args, stderr = TRUE)
    })
    if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
      # user canceled
      path = NA
    }
#   } else if (Sys.info()['sysname'] == 'Linux') {
#     command = 'zenity'
#     args = '--file-selection --directory --title="Choose a folder"'
#
#     suppressWarnings({
#       path = system2(command, args = args, stderr = TRUE)
#     })
#
#     #Return NA if user hits cancel
#     if (!is.null(attr(path, 'status')) && attr(path, 'status')) {
#       # user canceled
#       return(default)
#     }
#
#     #Error: Gtk-Message: GtkDialog mapped without a transient parent
#     if(length(path) == 2){
#       path = path[2]
#     }
#   }
  } else if (Sys.info()['sysname'] == 'Windows') {
    path <- try(file.choose())
    if (inherits(path, 'try-error')) {
      path <- NA
    }
  } else {
    path <- NA
  }

  return(path)
}

#' File Name Selection Control
#'
#' Create a file name selection control to select a file name on the server
#'
#' @param inputId The \code{input} slot that will be used to access the value
#' @param label Display label for the control, or NULL for no label
#' @param value Initial value.  Paths are exapnded via
#'   \code{\link{path.expand}}.
#'
#' @details This widget relies on \link{choose.filename}/ to present an
#'   interactive dialog to users for selecting a file name on the local filesystem.
#'   Therefore, this widget is intended for shiny apps that are run locally -
#'   i.e. on the same system that files/directories are to be accessed - and not
#'   from hosted applications (e.g. from shinyapps.io).
#'
#' @return A file name input control that can be added to a UI definition.
#'
#' @seealso \link{updateFileSelectInput}, \link{readFileSelectInput},
#'   \link{choose.file}, \link{choose.filename}
#' @export
filenameSelectInput <- function(inputId, label, value = NULL) {
  fileSelectInput(inputId, label, value)
}

#' Change the value of a filenameSelectInput on the client
#'
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#' @param inputId The id of the input object.
#' @param value A directory path to set
#' @param ... Additional arguments passed to \link{choose.filename}. Only
#'   used if \code{value} is \code{NULL}.
#'
#' @details Sends a message to the client, telling it to change the value of the
#' input object. For \code{filenameSelectInput} objects, this changes the value
#' displayed in the text-field and triggers a client-side change event. A
#' directory selection dialog is not displayed.
#' @export
updateFilenameSelectInput = function(session, inputId, value = NULL, ...) {
  if (is.null(value)) {
    value = choose.filename(...)
  }
  session$sendInputMessage(inputId, list(chosen_dir = value))
}

#' Read the value of a filenameSelectInput
#'
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#' @param inputId The id of the input object
#'
#' @details Reads the value of the text field associated with a
#' \code{filenameSelectInput} object that stores the user selected file path.
#' @export
readFilenameSelectInput = function(session, inputId) {
  session$input[[sprintf('%s__chosen_dir', inputId)]]
}
