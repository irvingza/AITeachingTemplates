# AI Usage Taxonomy Generator - R Shiny App
# Allows users to create customizable AI usage policy taxonomies

library(shiny)
library(shinyjs)
library(htmltools)

# Define colors for each permission level
colors <- list(
  never = list(fill = "#c62828", text = "Never Allowed"),
  sometimes = list(fill = "#6a1b9a", text = "Sometimes Allowed"),
  usually = list(fill = "#0288d1", text = "Usually Allowed"),
  none = list(fill = "transparent", text = "")
)

# Box layout constants
PARENT_BOX_WIDTH <- 320
CHILD_BOX_WIDTH <- 320
HEADER_HEIGHT <- 40
PADDING <- 12
SECTION_GAP <- 20
BLOCK_GAP <- 8
PERM_TEXT_OFFSET <- 20

# Font size presets: returns list(font_size, char_width, line_height)
# char_width and line_height scale proportionally so boxes resize correctly
# char_width is intentionally conservative (wider estimate) to prevent text overflow
get_font_params <- function(size_label = "small") {
  switch(size_label,
    "small"  = list(font_size = 12, char_width = 5.5, line_height = 14),
    "medium" = list(font_size = 14, char_width = 6.8, line_height = 17),
    "large"  = list(font_size = 16, char_width = 7.8, line_height = 20),
    # default to small
    list(font_size = 12, char_width = 5.5, line_height = 14)
  )
}

# Default category data using new description_blocks format
# Each description_block has: text, label (optional), label_color (optional: "never"/"sometimes"/"usually")
default_categories <- list(
  automation = list(
    title = "Automation",
    permission = "never",
    subtitle = "\"Interaction patterns focused on task completion.\"",
    description_blocks = list(
      list(text = "AI is driving the intellectual work (with or without your feedback) so this is always an honor violation",
           label = "Honor violation", label_color = "never")
    ),
    children = list(
      directive = list(
        title = "Directive Automation",
        permission = "never",
        subtitle = "\"Users give AI a task and it completes it with minimal back-and-forth\"",
        description_blocks = list(
          list(text = "Asking AI to write your outline, do your research, write a paragraph, a section, or your whole paper",
               label = "Examples", label_color = "never")
        )
      ),
      feedback_loops = list(
        title = "Feedback Loops",
        permission = "never",
        subtitle = "\"Users automate tasks and provide feedback to AI as needed\"",
        description_blocks = list(
          list(text = "Asking AI to write your outline, do your research, or write a paragraph, section, or the whole paper, then asking AI to redo the work after you read it.",
               label = "Examples", label_color = "never")
        )
      )
    )
  ),
  augmentation = list(
    title = "Augmentation",
    permission = "sometimes",
    subtitle = "\"Collaborative interaction patterns\"",
    description_blocks = list(
      list(text = "Allowed if you are driving the intellectual work, with feedback or teaching from AI.",
           label = "Allowed", label_color = "usually"),
      list(text = "Honor violation if AI is driving the intellectual work with your feedback.",
           label = "Honor violation", label_color = "never")
    ),
    children = list(
      task_iteration = list(
        title = "Task Iteration",
        permission = "sometimes",
        subtitle = "\"Users iterate on tasks collaboratively with AI.\"",
        description_blocks = list(
          list(text = "Can be allowed if you are doing the core intellectual work\u2014outlining, researching, writing\u2014AI is providing feedback, and you are adapting in response to feedback.",
               label = "Allowed", label_color = "usually"),
          list(text = "You make an outline \u2192 ask AI for feedback \u2192 you reflect on AI\u2019s criticisms and decide one is warranted \u2192 you go away and rewrite your outline in response \u2192 you have a back-and-forth conversation with AI about whether your response addresses the criticism.",
               label = "Good example", label_color = "usually"),
          list(text = "Honor violation if task iteration turns into feedback loops, where AI is driving the intellectual work.",
               label = "Honor violation", label_color = "never"),
          list(text = "You make an outline \u2192 ask AI for feedback \u2192 ask AI to rewrite outline in response to feedback \u2192 give AI feedback and ask to rewrite.",
               label = "Bad example", label_color = "never"),
          list(text = "Ask yourself, \u201cwho is doing the intellectual work, (vs providing feedback)?\u201d Answer must be, \u201cthe human, duh!\u201d",
               label = "Heuristic", label_color = "sometimes")
        )
      ),
      validation = list(
        title = "Validation",
        permission = "usually",
        subtitle = "\"Users ask AI for feedback on their work\"",
        description_blocks = list(
          list(text = "Can be a great way to develop your skills, so long as it does not turn into AI-driven feedback loops where you ask AI to redo your work and copy\u2013paste what it says.",
               label = "", label_color = "none"),
          list(text = "You ask AI for feedback on the logic of an argument you wrote, or for tips about how to improve your writing, and then revise your work on your own.",
               label = "Example", label_color = "usually")
        )
      ),
      learning = list(
        title = "Learning",
        permission = "usually",
        subtitle = "\"Users ask AI for information or explanations\"",
        description_blocks = list(
          list(text = "Usually allowed. Use AI to learn about various topics, ask questions to deepen your understanding.",
               label = "", label_color = "none"),
          list(text = "Asking AI to explain a concept, discussing ideas with AI, having AI teach you about a topic.",
               label = "Example", label_color = "usually")
        )
      )
    )
  )
)

# Escape HTML entities in text
escape_html <- function(text) {
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub('"', "&quot;", text)
  text <- gsub("'", "&#39;", text)
  text
}

# Count lines needed for wrapped text
count_wrapped_lines <- function(text, max_chars) {
  if (is.null(text) || nchar(trimws(text)) == 0) return(0)
  lines <- strsplit(text, "\n")[[1]]
  total <- 0
  for (line in lines) {
    if (nchar(trimws(line)) == 0) {
      total <- total + 0.7
      next
    }
    words <- strsplit(line, " ")[[1]]
    current_line <- ""
    for (word in words) {
      test_line <- if (nchar(current_line) == 0) word else paste(current_line, word)
      if (nchar(test_line) <= max_chars) {
        current_line <- test_line
      } else {
        if (nchar(current_line) > 0) total <- total + 1
        current_line <- word
      }
    }
    if (nchar(current_line) > 0) total <- total + 1
  }
  total
}

# Count lines with an inline label prefix on the first line
count_wrapped_lines_with_label <- function(text, max_chars, label_chars = 0) {
  if (is.null(text) || nchar(trimws(text)) == 0) {
    return(if (label_chars > 0) 1 else 0)
  }
  lines <- strsplit(text, "\n")[[1]]
  total <- 0
  first_content_line <- TRUE
  for (line in lines) {
    if (nchar(trimws(line)) == 0) {
      total <- total + 0.7
      next
    }
    words <- strsplit(line, " ")[[1]]
    current_len <- 0
    line_max <- if (first_content_line && label_chars > 0) max_chars - label_chars else max_chars
    first_content_line <- FALSE
    for (word in words) {
      test_len <- if (current_len == 0) nchar(word) else current_len + 1 + nchar(word)
      if (test_len <= line_max) {
        current_len <- test_len
      } else {
        if (current_len > 0) total <- total + 1
        current_len <- nchar(word)
        line_max <- max_chars  # subsequent lines get full width
      }
    }
    if (current_len > 0) total <- total + 1
  }
  total
}

# Calculate box height from its content
calc_box_height <- function(subtitle, description_blocks, box_width, fp) {
  max_chars <- floor((box_width - 2 * PADDING) / fp$char_width)

  # Header + permission text + gap to subtitle
  h <- HEADER_HEIGHT + PERM_TEXT_OFFSET + SECTION_GAP

  # Subtitle
  sub_lines <- count_wrapped_lines(subtitle, max_chars)
  h <- h + sub_lines * fp$line_height + BLOCK_GAP

  # Description blocks
  for (i in seq_along(description_blocks)) {
    block <- description_blocks[[i]]
    label <- if (!is.null(block$label) && nchar(trimws(block$label)) > 0) block$label else NULL

    # Label is inline with first line of text — it eats into the first line's char budget
    label_chars <- if (!is.null(label)) nchar(label) + 2 else 0  # +2 for ": "
    text_lines <- count_wrapped_lines_with_label(block$text, max_chars, label_chars)
    h <- h + text_lines * fp$line_height

    # Gap between blocks
    if (i < length(description_blocks)) {
      h <- h + BLOCK_GAP
    }
  }

  h <- h + PADDING  # bottom padding
  max(h, HEADER_HEIGHT + 60)
}

# Word wrap function for SVG text — returns list(tspans, y_end)
wrap_text_svg <- function(text, max_chars, y_start, x_pos, fill = "#333", line_height = 14) {
  if (is.null(text) || nchar(trimws(text)) == 0) return(list(tspans = "", y_end = y_start))

  lines <- strsplit(text, "\n")[[1]]
  result <- c()
  y_pos <- y_start

  for (line in lines) {
    if (nchar(trimws(line)) == 0) {
      y_pos <- y_pos + line_height * 0.7
      next
    }
    words <- strsplit(line, " ")[[1]]
    current_line <- ""

    for (word in words) {
      test_line <- if (nchar(current_line) == 0) word else paste(current_line, word)
      if (nchar(test_line) <= max_chars) {
        current_line <- test_line
      } else {
        if (nchar(current_line) > 0) {
          result <- c(result, sprintf('<tspan x="%d" y="%.1f" fill="%s">%s</tspan>',
                                      x_pos, y_pos, fill, escape_html(trimws(current_line))))
          y_pos <- y_pos + line_height
        }
        current_line <- word
      }
    }
    if (nchar(current_line) > 0) {
      result <- c(result, sprintf('<tspan x="%d" y="%.1f" fill="%s">%s</tspan>',
                                  x_pos, y_pos, fill, escape_html(trimws(current_line))))
      y_pos <- y_pos + line_height
    }
  }

  list(tspans = paste(result, collapse = "\n"), y_end = y_pos)
}

# Word wrap with an inline bold label prefix on the first line
wrap_text_svg_with_label <- function(text, max_chars, y_start, x_pos,
                                      fill = "#333", label_prefix = NULL, label_fill = "#333",
                                      line_height = 14) {
  if ((is.null(text) || nchar(trimws(text)) == 0) && is.null(label_prefix)) {
    return(list(tspans = "", y_end = y_start))
  }

  # If there's a label, the first line starts with the bold label tspan
  label_chars <- if (!is.null(label_prefix)) nchar(label_prefix) else 0

  # Combine text into words
  all_text <- if (!is.null(text)) text else ""
  lines <- strsplit(all_text, "\n")[[1]]
  result <- c()
  y_pos <- y_start
  first_content_line <- TRUE

  for (line in lines) {
    if (nchar(trimws(line)) == 0) {
      y_pos <- y_pos + line_height * 0.7
      next
    }
    words <- strsplit(line, " ")[[1]]
    current_line <- ""
    line_max <- if (first_content_line && label_chars > 0) max_chars - label_chars else max_chars

    for (word in words) {
      test_line <- if (nchar(current_line) == 0) word else paste(current_line, word)
      if (nchar(test_line) <= line_max) {
        current_line <- test_line
      } else {
        if (nchar(current_line) > 0) {
          if (first_content_line && !is.null(label_prefix)) {
            # First line: bold label + text on same line
            result <- c(result, sprintf(
              '<tspan x="%d" y="%.1f"><tspan fill="%s" font-weight="bold">%s</tspan><tspan fill="%s">%s</tspan></tspan>',
              x_pos, y_pos, label_fill, escape_html(label_prefix), fill,
              escape_html(trimws(current_line))))
            first_content_line <- FALSE
          } else {
            result <- c(result, sprintf('<tspan x="%d" y="%.1f" fill="%s">%s</tspan>',
                                        x_pos, y_pos, fill, escape_html(trimws(current_line))))
          }
          y_pos <- y_pos + line_height
        }
        current_line <- word
        line_max <- max_chars
      }
    }
    if (nchar(current_line) > 0) {
      if (first_content_line && !is.null(label_prefix)) {
        result <- c(result, sprintf(
          '<tspan x="%d" y="%.1f"><tspan fill="%s" font-weight="bold">%s</tspan><tspan fill="%s">%s</tspan></tspan>',
          x_pos, y_pos, label_fill, escape_html(label_prefix), fill,
          escape_html(trimws(current_line))))
        first_content_line <- FALSE
      } else {
        result <- c(result, sprintf('<tspan x="%d" y="%.1f" fill="%s">%s</tspan>',
                                    x_pos, y_pos, fill, escape_html(trimws(current_line))))
      }
      y_pos <- y_pos + line_height
    }
  }

  # If we still haven't emitted the label (empty text), emit it alone
  if (first_content_line && !is.null(label_prefix)) {
    result <- c(result, sprintf(
      '<tspan x="%d" y="%.1f" fill="%s" font-weight="bold">%s</tspan>',
      x_pos, y_pos, label_fill, escape_html(label_prefix)))
    y_pos <- y_pos + line_height
  }

  list(tspans = paste(result, collapse = "\n"), y_end = y_pos)
}

# Create SVG box element
create_box_svg <- function(id, x, y, width, height, title, permission, subtitle, description_blocks, fp) {
  color <- colors[[permission]]$fill
  perm_text <- colors[[permission]]$text
  max_chars <- floor((width - 2 * PADDING) / fp$char_width)
  x_text <- x + PADDING

  svg_parts <- c()

  # Drop shadow filter
  filter_id <- paste0("shadow-", id)
  svg_parts <- c(svg_parts, sprintf('
  <defs>
    <filter id="%s" x="-20%%" y="-20%%" width="140%%" height="140%%">
      <feOffset dx="-4" dy="4"/>
      <feGaussianBlur result="blur" stdDeviation="5"/>
      <feFlood flood-color="#000" flood-opacity=".3"/>
      <feComposite in2="blur" operator="in"/>
      <feComposite in="SourceGraphic"/>
    </filter>
  </defs>', filter_id))

  # Main container with shadow
  svg_parts <- c(svg_parts, sprintf('
  <g filter="url(#%s)">
    <rect x="%d" y="%d" width="%d" height="%d" rx="8" ry="8" fill="#f0f0f0"/>
    <rect x="%d" y="%d" width="%d" height="%d" rx="8" ry="0" fill="%s"/>
  </g>',
    filter_id, x, y, width, round(height), x, y, width, HEADER_HEIGHT, color))

  # Title text
  svg_parts <- c(svg_parts, sprintf('
  <text x="%d" y="%d" fill="white" font-family="Libre Franklin, Arial, sans-serif" font-size="16" font-weight="bold" text-anchor="middle">%s</text>',
    x + width/2, y + 26, escape_html(title)))

  # Permission text
  cur_y <- y + HEADER_HEIGHT + PERM_TEXT_OFFSET
  svg_parts <- c(svg_parts, sprintf('
  <text x="%d" y="%.1f" fill="%s" font-family="Libre Franklin, Arial, sans-serif" font-size="%d" font-style="italic" text-anchor="middle">%s</text>',
    x + width/2, cur_y, color, fp$font_size, escape_html(perm_text)))

  cur_y <- cur_y + SECTION_GAP

  # Subtitle text (italic, wrapped)
  sub_result <- wrap_text_svg(subtitle, max_chars, cur_y, x_text, fill = "#555",
                               line_height = fp$line_height)
  if (nchar(sub_result$tspans) > 0) {
    svg_parts <- c(svg_parts, sprintf('
  <text font-family="Libre Franklin, Arial, sans-serif" font-size="%d" font-style="italic">%s</text>',
      fp$font_size, sub_result$tspans))
  }
  cur_y <- sub_result$y_end + BLOCK_GAP

  # Description blocks
  for (i in seq_along(description_blocks)) {
    block <- description_blocks[[i]]
    label <- if (!is.null(block$label) && nchar(trimws(block$label)) > 0) block$label else NULL
    label_color_name <- if (!is.null(block$label_color)) block$label_color else "none"
    label_fill <- if (label_color_name != "none" && !is.null(colors[[label_color_name]])) {
      colors[[label_color_name]]$fill
    } else {
      "#333"
    }

    # Render label inline with text (like LaTeX description environment)
    label_prefix <- if (!is.null(label)) paste0(label, ": ") else NULL
    block_result <- wrap_text_svg_with_label(block$text, max_chars, cur_y, x_text,
                                              label_prefix = label_prefix,
                                              label_fill = label_fill,
                                              line_height = fp$line_height)
    if (nchar(block_result$tspans) > 0) {
      svg_parts <- c(svg_parts, sprintf(
        '<text font-family="Libre Franklin, Arial, sans-serif" font-size="%d">%s</text>',
        fp$font_size, block_result$tspans))
    }
    cur_y <- block_result$y_end

    if (i < length(description_blocks)) {
      cur_y <- cur_y + BLOCK_GAP
    }
  }

  paste(svg_parts, collapse = "")
}

# Generate full SVG from category data
generate_taxonomy_svg <- function(categories, main_title = "AI Usage", main_subtitle = "A Taxonomy",
                                   font_size_label = "small") {

  fp <- get_font_params(font_size_label)

  cat_names <- names(categories)
  n_cats <- length(cat_names)

  # Compute all child box heights first so we can determine overall SVG size
  # Parent row Y
  parent_y <- 150

  # Compute parent heights
  parent_heights <- c()
  for (i in seq_along(cat_names)) {
    cat_data <- categories[[cat_names[i]]]
    ph <- calc_box_height(cat_data$subtitle, cat_data$description_blocks, PARENT_BOX_WIDTH, fp)
    parent_heights <- c(parent_heights, ph)
  }
  max_parent_height <- max(parent_heights)

  # Child row Y
  connector_gap <- 60
  child_y <- parent_y + max_parent_height + connector_gap

  # Compute child heights and collect child info
  child_info_list <- list()  # list of lists per parent
  max_child_height <- 0
  total_children <- 0

  for (i in seq_along(cat_names)) {
    cat_data <- categories[[cat_names[i]]]
    children <- cat_data$children
    child_info <- list()
    if (!is.null(children) && length(children) > 0) {
      for (j in seq_along(children)) {
        ch <- children[[j]]
        ch_height <- calc_box_height(ch$subtitle, ch$description_blocks, CHILD_BOX_WIDTH, fp)
        child_info[[j]] <- list(data = ch, height = ch_height, name = names(children)[j])
        max_child_height <- max(max_child_height, ch_height)
        total_children <- total_children + 1
      }
    }
    child_info_list[[i]] <- child_info
  }

  # Layout: distribute children evenly
  child_gap <- 20

  # Compute total width needed
  # Each child occupies CHILD_BOX_WIDTH + child_gap
  total_width_needed <- total_children * CHILD_BOX_WIDTH + (total_children - 1) * child_gap

  # Also account for parent boxes — parents should be centered over their children
  # We'll compute child x positions first, then derive parent positions

  parent_gap <- 60  # minimum gap between parent groups of children

  # Compute x positions for each child, grouped by parent
  child_x_positions <- list()
  cur_x <- 40  # left margin
  parent_centers <- c()

  for (i in seq_along(cat_names)) {
    n_ch <- length(child_info_list[[i]])
    group_positions <- c()
    if (n_ch > 0) {
      for (j in seq_len(n_ch)) {
        group_positions <- c(group_positions, cur_x)
        cur_x <- cur_x + CHILD_BOX_WIDTH + child_gap
      }
      # Center of this group
      group_center <- (group_positions[1] + group_positions[n_ch] + CHILD_BOX_WIDTH) / 2
      parent_centers <- c(parent_centers, group_center)
      cur_x <- cur_x - child_gap + parent_gap  # replace last child_gap with parent_gap
    } else {
      # No children — place parent at current position
      parent_centers <- c(parent_centers, cur_x + PARENT_BOX_WIDTH / 2)
      cur_x <- cur_x + PARENT_BOX_WIDTH + parent_gap
    }
    child_x_positions[[i]] <- group_positions
  }

  # Parent x positions (centered over children)
  parent_x_positions <- c()
  for (i in seq_along(cat_names)) {
    parent_x_positions <- c(parent_x_positions, parent_centers[i] - PARENT_BOX_WIDTH / 2)
  }

  # SVG dimensions
  svg_width <- max(cur_x + 20, max(parent_x_positions) + PARENT_BOX_WIDTH + 40)
  svg_height <- child_y + max_child_height + 40

  # Main title box — auto-size to fit the wider of title or subtitle
  TITLE_CHAR_WIDTH <- 10    # approx px per char at font-size 18 bold
  SUBTITLE_CHAR_WIDTH <- 7  # approx px per char at font-size 14 italic
  TITLE_H_PAD <- 30         # horizontal padding (both sides combined)
  title_text_width <- nchar(main_title) * TITLE_CHAR_WIDTH
  subtitle_text_width <- nchar(main_subtitle) * SUBTITLE_CHAR_WIDTH
  title_width <- max(160, max(title_text_width, subtitle_text_width) + TITLE_H_PAD)
  title_height <- 78
  title_x <- svg_width / 2 - title_width / 2
  title_y <- 22

  # Start SVG
  svg_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %d %d" width="%d" height="%d">
  <style>
    @import url("https://fonts.googleapis.com/css2?family=Libre+Franklin:ital,wght@0,400;0,600;0,700;1,400&amp;display=swap");
    .connector { stroke: #bdc3c7; stroke-dasharray: 5 5; stroke-width: 2; fill: none; }
  </style>

  <!-- Background -->
  <rect width="100%%" height="100%%" fill="white"/>
', round(svg_width), round(svg_height), round(svg_width), round(svg_height))

  # Main title box
  svg_content <- paste0(svg_content, sprintf('
  <!-- Main Title Box -->
  <g>
    <filter id="shadow-main" x="-10%%" y="-10%%" width="120%%" height="120%%">
      <feOffset dx="-4" dy="4"/>
      <feGaussianBlur result="blur" stdDeviation="5"/>
      <feFlood flood-color="#000" flood-opacity=".3"/>
      <feComposite in2="blur" operator="in"/>
      <feComposite in="SourceGraphic"/>
    </filter>
    <g filter="url(#shadow-main)">
      <rect x="%d" y="%d" width="%d" height="%d" rx="8" ry="8" fill="#f0f0f0"/>
      <rect x="%d" y="%d" width="%d" height="30" rx="8" ry="0" fill="#6a1b9a"/>
    </g>
    <text x="%d" y="%d" fill="white" font-family="Libre Franklin, Arial, sans-serif" font-size="18" font-weight="bold" text-anchor="middle">%s</text>
    <text x="%d" y="%d" fill="#666" font-family="Libre Franklin, Arial, sans-serif" font-size="14" font-style="italic" text-anchor="middle">%s</text>
  </g>
', round(title_x), title_y, title_width, title_height,
   round(title_x), title_y, title_width,
   round(title_x + title_width/2), title_y + 22, escape_html(main_title),
   round(title_x + title_width/2), title_y + 55, escape_html(main_subtitle)))

  # Connector from main title down
  main_center <- title_x + title_width / 2
  branch_y <- title_y + title_height + 15
  svg_content <- paste0(svg_content, sprintf('
  <line class="connector" x1="%d" y1="%d" x2="%d" y2="%d"/>
', round(main_center), title_y + title_height, round(main_center), round(branch_y)))

  # Horizontal branch line connecting parents
  if (n_cats > 1) {
    svg_content <- paste0(svg_content, sprintf('
  <line class="connector" x1="%d" y1="%d" x2="%d" y2="%d"/>
', round(parent_centers[1]), round(branch_y), round(parent_centers[n_cats]), round(branch_y)))
  }

  # Draw parents and children
  box_counter <- 0

  for (i in seq_along(cat_names)) {
    cat_data <- categories[[cat_names[i]]]
    box_counter <- box_counter + 1
    px <- parent_x_positions[i]
    py <- parent_y
    ph <- parent_heights[i]
    pc <- parent_centers[i]

    # Vertical connector from branch to parent
    svg_content <- paste0(svg_content, sprintf('
  <line class="connector" x1="%d" y1="%d" x2="%d" y2="%d"/>
', round(pc), round(branch_y), round(pc), round(py)))

    # Draw parent box
    svg_content <- paste0(svg_content, create_box_svg(
      id = paste0("cat-", box_counter),
      x = round(px), y = round(py), width = PARENT_BOX_WIDTH, height = ph,
      title = cat_data$title,
      permission = cat_data$permission,
      subtitle = cat_data$subtitle,
      description_blocks = cat_data$description_blocks,
      fp = fp
    ))

    # Children
    children_info <- child_info_list[[i]]
    n_ch <- length(children_info)
    if (n_ch > 0) {
      child_branch_y <- py + max_parent_height + connector_gap / 2

      # Vertical connector from parent to child branch
      svg_content <- paste0(svg_content, sprintf('
  <line class="connector" x1="%d" y1="%d" x2="%d" y2="%d"/>
', round(pc), round(py + ph), round(pc), round(child_branch_y)))

      # Horizontal connector between children
      child_xs <- child_x_positions[[i]]
      if (n_ch > 1) {
        left_center <- child_xs[1] + CHILD_BOX_WIDTH / 2
        right_center <- child_xs[n_ch] + CHILD_BOX_WIDTH / 2
        svg_content <- paste0(svg_content, sprintf('
  <line class="connector" x1="%d" y1="%d" x2="%d" y2="%d"/>
', round(left_center), round(child_branch_y), round(right_center), round(child_branch_y)))
      }

      for (j in seq_along(children_info)) {
        ch_info <- children_info[[j]]
        box_counter <- box_counter + 1
        cx <- child_xs[j]
        cy <- child_y
        ch_height <- ch_info$height
        ch_center <- cx + CHILD_BOX_WIDTH / 2

        # Vertical connector to child
        svg_content <- paste0(svg_content, sprintf('
  <line class="connector" x1="%d" y1="%d" x2="%d" y2="%d"/>
', round(ch_center), round(child_branch_y), round(ch_center), round(cy)))

        svg_content <- paste0(svg_content, create_box_svg(
          id = paste0("child-", box_counter),
          x = round(cx), y = round(cy), width = CHILD_BOX_WIDTH, height = ch_height,
          title = ch_info$data$title,
          permission = ch_info$data$permission,
          subtitle = ch_info$data$subtitle,
          description_blocks = ch_info$data$description_blocks,
          fp = fp
        ))
      }
    }
  }

  svg_content <- paste0(svg_content, "\n</svg>")
  svg_content
}

# Generate CSV from category data for save/reload
generate_taxonomy_csv <- function(categories, main_title = "AI Usage", main_subtitle = "A Taxonomy") {
  rows <- list()

  cat_names <- names(categories)
  for (i in seq_along(cat_names)) {
    cat_id <- cat_names[i]
    cat <- categories[[cat_id]]

    # Category-level description blocks
    if (!is.null(cat$description_blocks) && length(cat$description_blocks) > 0) {
      for (b in seq_along(cat$description_blocks)) {
        block <- cat$description_blocks[[b]]
        rows[[length(rows) + 1]] <- data.frame(
          main_title = main_title,
          main_subtitle = main_subtitle,
          category_id = cat_id,
          category_title = cat$title,
          category_permission = cat$permission,
          category_subtitle = cat$subtitle,
          child_id = "",
          child_title = "",
          child_permission = "",
          child_subtitle = "",
          block_index = b,
          block_label = if (!is.null(block$label)) block$label else "",
          block_label_color = if (!is.null(block$label_color)) block$label_color else "none",
          block_text = if (!is.null(block$text)) block$text else "",
          stringsAsFactors = FALSE
        )
      }
    } else {
      rows[[length(rows) + 1]] <- data.frame(
        main_title = main_title,
        main_subtitle = main_subtitle,
        category_id = cat_id,
        category_title = cat$title,
        category_permission = cat$permission,
        category_subtitle = cat$subtitle,
        child_id = "",
        child_title = "",
        child_permission = "",
        child_subtitle = "",
        block_index = 0,
        block_label = "",
        block_label_color = "",
        block_text = "",
        stringsAsFactors = FALSE
      )
    }

    # Children
    if (!is.null(cat$children)) {
      child_names <- names(cat$children)
      for (j in seq_along(child_names)) {
        ch_id <- child_names[j]
        ch <- cat$children[[ch_id]]

        if (!is.null(ch$description_blocks) && length(ch$description_blocks) > 0) {
          for (b in seq_along(ch$description_blocks)) {
            block <- ch$description_blocks[[b]]
            rows[[length(rows) + 1]] <- data.frame(
              main_title = main_title,
              main_subtitle = main_subtitle,
              category_id = cat_id,
              category_title = cat$title,
              category_permission = cat$permission,
              category_subtitle = cat$subtitle,
              child_id = ch_id,
              child_title = ch$title,
              child_permission = ch$permission,
              child_subtitle = ch$subtitle,
              block_index = b,
              block_label = if (!is.null(block$label)) block$label else "",
              block_label_color = if (!is.null(block$label_color)) block$label_color else "none",
              block_text = if (!is.null(block$text)) block$text else "",
              stringsAsFactors = FALSE
            )
          }
        } else {
          rows[[length(rows) + 1]] <- data.frame(
            main_title = main_title,
            main_subtitle = main_subtitle,
            category_id = cat_id,
            category_title = cat$title,
            category_permission = cat$permission,
            category_subtitle = cat$subtitle,
            child_id = ch_id,
            child_title = ch$title,
            child_permission = ch$permission,
            child_subtitle = ch$subtitle,
            block_index = 0,
            block_label = "",
            block_label_color = "",
            block_text = "",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  do.call(rbind, rows)
}

# Parse a saved CSV back into the categories list structure
parse_taxonomy_csv <- function(df) {
  # Returns list(main_title, main_subtitle, categories)
  main_title <- df$main_title[1]
  main_subtitle <- df$main_subtitle[1]

  categories <- list()
  cat_ids <- unique(df$category_id)

  for (cid in cat_ids) {
    cat_rows <- df[df$category_id == cid, , drop = FALSE]
    cat_title <- cat_rows$category_title[1]
    cat_perm <- cat_rows$category_permission[1]
    cat_sub <- cat_rows$category_subtitle[1]

    # Category-level blocks (rows where child_id is empty)
    cat_block_rows <- cat_rows[cat_rows$child_id == "", , drop = FALSE]
    cat_blocks <- list()
    if (nrow(cat_block_rows) > 0) {
      for (r in seq_len(nrow(cat_block_rows))) {
        row <- cat_block_rows[r, ]
        if (row$block_index > 0) {
          cat_blocks[[length(cat_blocks) + 1]] <- list(
            text = as.character(row$block_text),
            label = as.character(row$block_label),
            label_color = as.character(row$block_label_color)
          )
        }
      }
    }
    if (length(cat_blocks) == 0) {
      cat_blocks <- list(list(text = "", label = "", label_color = "none"))
    }

    # Children
    child_rows <- cat_rows[cat_rows$child_id != "", , drop = FALSE]
    children <- list()
    if (nrow(child_rows) > 0) {
      child_ids <- unique(child_rows$child_id)
      for (chid in child_ids) {
        ch_rows <- child_rows[child_rows$child_id == chid, , drop = FALSE]
        ch_title <- ch_rows$child_title[1]
        ch_perm <- ch_rows$child_permission[1]
        ch_sub <- ch_rows$child_subtitle[1]

        ch_blocks <- list()
        for (r in seq_len(nrow(ch_rows))) {
          row <- ch_rows[r, ]
          if (row$block_index > 0) {
            ch_blocks[[length(ch_blocks) + 1]] <- list(
              text = as.character(row$block_text),
              label = as.character(row$block_label),
              label_color = as.character(row$block_label_color)
            )
          }
        }
        if (length(ch_blocks) == 0) {
          ch_blocks <- list(list(text = "", label = "", label_color = "none"))
        }

        children[[as.character(chid)]] <- list(
          title = as.character(ch_title),
          permission = as.character(ch_perm),
          subtitle = as.character(ch_sub),
          description_blocks = ch_blocks
        )
      }
    }

    categories[[as.character(cid)]] <- list(
      title = as.character(cat_title),
      permission = as.character(cat_perm),
      subtitle = as.character(cat_sub),
      description_blocks = cat_blocks,
      children = children
    )
  }

  list(main_title = as.character(main_title),
       main_subtitle = as.character(main_subtitle),
       categories = categories)
}

# ----- UI helper: generate description block inputs for a card -----
desc_block_ui <- function(card_id, block_index, label_val = "", label_color_val = "none", text_val = "") {
  block_id <- paste0(card_id, "_block_", block_index)
  div(
    class = "desc-block",
    fluidRow(
      column(6, textInput(paste0(block_id, "_label"), "Block Label (optional)",
                          value = label_val, width = "100%")),
      column(6, selectInput(paste0(block_id, "_label_color"), "Label Color",
                            choices = c("None" = "none",
                                        "Red (Never)" = "never",
                                        "Purple (Sometimes)" = "sometimes",
                                        "Blue (Usually)" = "usually"),
                            selected = label_color_val, width = "100%"))
    ),
    textAreaInput(paste0(block_id, "_text"), NULL, value = text_val, rows = 2, width = "100%"),
    actionButton(paste0(block_id, "_remove"), "Remove Block",
                 class = "btn btn-xs btn-danger", style = "margin-bottom:8px;"),
    hr(style = "margin:4px 0;")
  )
}

# UI
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Libre+Franklin:ital,wght@0,400;0,600;0,700;1,400&display=swap"),
    tags$style(HTML("
      body { font-family: 'Libre Franklin', Arial, sans-serif; }
      .category-panel {
        background: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
      .child-panel {
        background: #fff;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        padding: 12px;
        margin: 10px 0;
        margin-left: 20px;
      }
      .permission-never { border-left: 4px solid #c62828; }
      .permission-sometimes { border-left: 4px solid #6a1b9a; }
      .permission-usually { border-left: 4px solid #0288d1; }
      .preview-container {
        background: white;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 10px;
        overflow: auto;
        max-height: 800px;
      }
      h4 { color: #333; margin-bottom: 15px; }
      .btn-primary { background-color: #6a1b9a; border-color: #6a1b9a; }
      .btn-primary:hover { background-color: #4a148c; border-color: #4a148c; }
      .desc-block { background: #f9f9f9; border: 1px solid #eee; border-radius: 4px; padding: 8px; margin-bottom: 6px; }
      .btn-xs { padding: 2px 8px; font-size: 11px; }
    "))
  ),

  titlePanel("AI Usage Taxonomy Generator"),

  sidebarLayout(
    sidebarPanel(
      width = 5,

      # Action buttons grouped at the top
      fluidRow(
        column(6, actionButton("generate", "Generate Taxonomy", class = "btn-primary", width = "100%")),
        column(6, fileInput("upload_csv", NULL, accept = ".csv", width = "100%",
                            buttonLabel = "Load CSV", placeholder = "No file selected"))
      ),
      fluidRow(
        column(6, downloadButton("save_taxonomy", "Save Taxonomy", class = "btn btn-success", style = "width: 100%;")),
        column(6, actionButton("reset_data", "Reset (delete data)", class = "btn btn-danger", width = "100%"))
      ),
      
      fluidRow(
        column(12, 
               tags$a(
                 href = "https://forms.gle/dTkNaaMYBaJrUdzh9", 
                 target = "_blank", # Opens in a new tab
                 "Provide Feedback (Form)", 
                 class = "btn btn-info", 
                 style = "width: 100%; margin-top: 10px;"
               ))
        ),
      hr(),

      h4("Main Title"),
      textInput("main_title", NULL, value = "AI Usage", width = "100%"),
      textInput("main_subtitle", "Subtitle", value = "A Taxonomy", width = "100%"),

      selectInput("font_size", "Body Text Size",
                  choices = c("Small (12pt)" = "small",
                              "Medium (14pt)" = "medium",
                              "Large (16pt)" = "large"),
                  selected = "small", width = "100%"),

      hr(),

      # We'll render the full category editor dynamically
      uiOutput("category_editor")
    ),

    mainPanel(
      width = 7,
      h4("Preview"),
      div(class = "preview-container",
          uiOutput("svg_preview")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive value to store generated SVG
  svg_content <- reactiveVal("")

  # ---- Reactive data store for categories ----
  # We store as a reactiveValues list. Each category has: title, permission, subtitle, description_blocks, children
  # Each child has: title, permission, subtitle, description_blocks

  cat_data <- reactiveValues()

  # Initialize from defaults
  observe({
    if (is.null(cat_data$categories)) {
      cat_data$categories <- default_categories
    }
  }, priority = 100)

  # ---- Render the category editor UI dynamically ----
  output$category_editor <- renderUI({
    cats <- cat_data$categories
    if (is.null(cats)) return(NULL)

    cat_names <- names(cats)
    ui_list <- list()

    for (i in seq_along(cat_names)) {
      cat_id <- cat_names[i]
      cat <- cats[[cat_id]]
      perm_class <- paste0("permission-", cat$permission)

      # Description blocks for this category
      blocks_ui <- list()
      if (!is.null(cat$description_blocks)) {
        for (b in seq_along(cat$description_blocks)) {
          block <- cat$description_blocks[[b]]
          bid <- paste0(cat_id, "_block_", b)
          blocks_ui[[b]] <- div(
            class = "desc-block",
            fluidRow(
              column(6, textInput(paste0(bid, "_label"), "Block Label",
                                  value = if(!is.null(block$label)) block$label else "", width = "100%")),
              column(6, selectInput(paste0(bid, "_label_color"), "Label Color",
                                    choices = c("None" = "none",
                                                "Red (Never)" = "never",
                                                "Purple (Sometimes)" = "sometimes",
                                                "Blue (Usually)" = "usually"),
                                    selected = if(!is.null(block$label_color)) block$label_color else "none",
                                    width = "100%"))
            ),
            textAreaInput(paste0(bid, "_text"), NULL,
                          value = if(!is.null(block$text)) block$text else "", rows = 2, width = "100%"),
            actionButton(paste0(bid, "_remove"), "Remove", class = "btn btn-xs btn-danger",
                         style = "margin-bottom:4px;")
          )
        }
      }

      # Children
      children_ui <- list()
      if (!is.null(cat$children)) {
        child_names <- names(cat$children)
        for (j in seq_along(child_names)) {
          ch_id <- paste0(cat_id, "_child_", child_names[j])
          ch <- cat$children[[child_names[j]]]
          ch_perm_class <- paste0("permission-", ch$permission)

          ch_blocks_ui <- list()
          if (!is.null(ch$description_blocks)) {
            for (b in seq_along(ch$description_blocks)) {
              block <- ch$description_blocks[[b]]
              cbid <- paste0(ch_id, "_block_", b)
              ch_blocks_ui[[b]] <- div(
                class = "desc-block",
                fluidRow(
                  column(6, textInput(paste0(cbid, "_label"), "Block Label",
                                      value = if(!is.null(block$label)) block$label else "", width = "100%")),
                  column(6, selectInput(paste0(cbid, "_label_color"), "Label Color",
                                        choices = c("None" = "none",
                                                    "Red (Never)" = "never",
                                                    "Purple (Sometimes)" = "sometimes",
                                                    "Blue (Usually)" = "usually"),
                                        selected = if(!is.null(block$label_color)) block$label_color else "none",
                                        width = "100%"))
                ),
                textAreaInput(paste0(cbid, "_text"), NULL,
                              value = if(!is.null(block$text)) block$text else "", rows = 2, width = "100%"),
                actionButton(paste0(cbid, "_remove"), "Remove", class = "btn btn-xs btn-danger",
                             style = "margin-bottom:4px;")
              )
            }
          }

          children_ui[[j]] <- tagList(
            h5(paste0("Child: ", ch$title)),
            div(class = paste("child-panel", ch_perm_class),
                textInput(paste0(ch_id, "_title"), "Title", value = ch$title, width = "100%"),
                selectInput(paste0(ch_id, "_permission"), "Permission",
                            choices = c("Never Allowed" = "never",
                                        "Sometimes Allowed" = "sometimes",
                                        "Usually Allowed" = "usually"),
                            selected = ch$permission, width = "100%"),
                textInput(paste0(ch_id, "_subtitle"), "Subtitle", value = ch$subtitle, width = "100%"),
                h6("Description Blocks"),
                ch_blocks_ui,
                actionButton(paste0(ch_id, "_add_block"), "Add Description Block",
                             class = "btn btn-xs btn-info", style = "margin-top:4px;")
            )
          )
        }
      }

      ui_list[[i]] <- tagList(
        h4(paste0("Category: ", cat$title)),
        div(class = paste("category-panel", perm_class),
            textInput(paste0(cat_id, "_title"), "Title", value = cat$title, width = "100%"),
            selectInput(paste0(cat_id, "_permission"), "Permission Level",
                        choices = c("Never Allowed" = "never",
                                    "Sometimes Allowed" = "sometimes",
                                    "Usually Allowed" = "usually"),
                        selected = cat$permission, width = "100%"),
            textInput(paste0(cat_id, "_subtitle"), "Subtitle", value = cat$subtitle, width = "100%"),
            h6("Description Blocks"),
            blocks_ui,
            actionButton(paste0(cat_id, "_add_block"), "Add Description Block",
                         class = "btn btn-xs btn-info", style = "margin-top:4px;"),
            children_ui
        ),
        hr()
      )
    }

    do.call(tagList, ui_list)
  })

  # ---- Observe add/remove block buttons ----
  # Track which observer IDs have been registered to avoid stacking duplicates
  registered_observers <- reactiveVal(character(0))

  observe({
    cats <- cat_data$categories
    if (is.null(cats)) return()

    already <- registered_observers()
    new_ids <- character(0)

    cat_names <- names(cats)
    for (i in seq_along(cat_names)) {
      local({
        ci <- i
        cat_id <- cat_names[ci]

        # Add block button for category
        add_id <- paste0(cat_id, "_add_block")
        if (!(add_id %in% already)) {
          new_ids <<- c(new_ids, add_id)
          observeEvent(input[[add_id]], {
            cats <- sync_categories_from_inputs()
            n <- length(cats[[cat_id]]$description_blocks)
            cats[[cat_id]]$description_blocks[[n + 1]] <- list(text = "", label = "", label_color = "none")
            cat_data$categories <- cats
          }, ignoreInit = TRUE)
        }

        # Remove block buttons for category
        cat <- cats[[cat_id]]
        if (!is.null(cat$description_blocks)) {
          for (b in seq_along(cat$description_blocks)) {
            local({
              bi <- b
              rm_id <- paste0(cat_id, "_block_", bi, "_remove")
              if (!(rm_id %in% already)) {
                new_ids <<- c(new_ids, rm_id)
                observeEvent(input[[rm_id]], {
                  cats <- sync_categories_from_inputs()
                  blocks <- cats[[cat_id]]$description_blocks
                  if (length(blocks) > 1) {
                    blocks[[bi]] <- NULL
                    cats[[cat_id]]$description_blocks <- blocks
                    cat_data$categories <- cats
                  }
                }, ignoreInit = TRUE)
              }
            })
          }
        }

        # Children
        if (!is.null(cat$children)) {
          child_names <- names(cat$children)
          for (j in seq_along(child_names)) {
            local({
              cj <- j
              ch_id <- paste0(cat_id, "_child_", child_names[cj])

              # Add block button for child
              ch_add_id <- paste0(ch_id, "_add_block")
              if (!(ch_add_id %in% already)) {
                new_ids <<- c(new_ids, ch_add_id)
                observeEvent(input[[ch_add_id]], {
                  cats <- sync_categories_from_inputs()
                  ch <- cats[[cat_id]]$children[[child_names[cj]]]
                  n <- length(ch$description_blocks)
                  ch$description_blocks[[n + 1]] <- list(text = "", label = "", label_color = "none")
                  cats[[cat_id]]$children[[child_names[cj]]] <- ch
                  cat_data$categories <- cats
                }, ignoreInit = TRUE)
              }

              # Remove block buttons for child
              ch <- cat$children[[child_names[cj]]]
              if (!is.null(ch$description_blocks)) {
                for (b in seq_along(ch$description_blocks)) {
                  local({
                    bi <- b
                    ch_rm_id <- paste0(ch_id, "_block_", bi, "_remove")
                    if (!(ch_rm_id %in% already)) {
                      new_ids <<- c(new_ids, ch_rm_id)
                      observeEvent(input[[ch_rm_id]], {
                        cats <- sync_categories_from_inputs()
                        blocks <- cats[[cat_id]]$children[[child_names[cj]]]$description_blocks
                        if (length(blocks) > 1) {
                          blocks[[bi]] <- NULL
                          cats[[cat_id]]$children[[child_names[cj]]]$description_blocks <- blocks
                          cat_data$categories <- cats
                        }
                      }, ignoreInit = TRUE)
                    }
                  })
                }
              }
            })
          }
        }
      })
    }

    # Update the registry with all newly registered IDs
    if (length(new_ids) > 0) {
      isolate(registered_observers(c(already, new_ids)))
    }
  })

  # ---- Sync current input values back into cat_data$categories ----
  # Non-reactive helper: reads all current input values and returns an updated categories list.
  # Call this before any mutation (add/remove block) so the UI rebuild preserves user edits.
  sync_categories_from_inputs <- function() {
    cats <- cat_data$categories
    if (is.null(cats)) return(default_categories)

    cat_names <- names(cats)
    result <- list()

    for (i in seq_along(cat_names)) {
      cat_id <- cat_names[i]
      cat <- cats[[cat_id]]

      title_val <- input[[paste0(cat_id, "_title")]]
      perm_val <- input[[paste0(cat_id, "_permission")]]
      sub_val <- input[[paste0(cat_id, "_subtitle")]]

      # Read description blocks from inputs
      blocks <- list()
      if (!is.null(cat$description_blocks)) {
        for (b in seq_along(cat$description_blocks)) {
          bid <- paste0(cat_id, "_block_", b)
          orig <- cat$description_blocks[[b]]
          lbl <- input[[paste0(bid, "_label")]]
          lbl_col <- input[[paste0(bid, "_label_color")]]
          txt <- input[[paste0(bid, "_text")]]
          blocks[[b]] <- list(
            text = if (!is.null(txt)) txt else if (!is.null(orig$text)) orig$text else "",
            label = if (!is.null(lbl)) lbl else if (!is.null(orig$label)) orig$label else "",
            label_color = if (!is.null(lbl_col)) lbl_col else if (!is.null(orig$label_color)) orig$label_color else "none"
          )
        }
      }

      # Children
      children <- list()
      if (!is.null(cat$children)) {
        child_names <- names(cat$children)
        for (j in seq_along(child_names)) {
          ch_id <- paste0(cat_id, "_child_", child_names[j])
          ch <- cat$children[[child_names[j]]]

          ch_title <- input[[paste0(ch_id, "_title")]]
          ch_perm <- input[[paste0(ch_id, "_permission")]]
          ch_sub <- input[[paste0(ch_id, "_subtitle")]]

          ch_blocks <- list()
          if (!is.null(ch$description_blocks)) {
            for (b in seq_along(ch$description_blocks)) {
              cbid <- paste0(ch_id, "_block_", b)
              orig_ch <- ch$description_blocks[[b]]
              lbl <- input[[paste0(cbid, "_label")]]
              lbl_col <- input[[paste0(cbid, "_label_color")]]
              txt <- input[[paste0(cbid, "_text")]]
              ch_blocks[[b]] <- list(
                text = if (!is.null(txt)) txt else if (!is.null(orig_ch$text)) orig_ch$text else "",
                label = if (!is.null(lbl)) lbl else if (!is.null(orig_ch$label)) orig_ch$label else "",
                label_color = if (!is.null(lbl_col)) lbl_col else if (!is.null(orig_ch$label_color)) orig_ch$label_color else "none"
              )
            }
          }

          children[[child_names[j]]] <- list(
            title = if (!is.null(ch_title)) ch_title else ch$title,
            permission = if (!is.null(ch_perm)) ch_perm else ch$permission,
            subtitle = if (!is.null(ch_sub)) ch_sub else ch$subtitle,
            description_blocks = ch_blocks
          )
        }
      }

      result[[cat_id]] <- list(
        title = if (!is.null(title_val)) title_val else cat$title,
        permission = if (!is.null(perm_val)) perm_val else cat$permission,
        subtitle = if (!is.null(sub_val)) sub_val else cat$subtitle,
        description_blocks = blocks,
        children = children
      )
    }

    result
  }

  # ---- Build categories from current input values (reactive version) ----
  build_categories <- reactive({
    sync_categories_from_inputs()
  })

  # Generate SVG on button click
  observeEvent(input$generate, {
    categories <- build_categories()
    svg <- generate_taxonomy_svg(categories, input$main_title, input$main_subtitle,
                                  font_size_label = input$font_size)
    svg_content(svg)
  }, ignoreNULL = FALSE)

  # Initial generation
  observe({
    if (svg_content() == "") {
      categories <- build_categories()
      svg <- generate_taxonomy_svg(categories, input$main_title, input$main_subtitle,
                                    font_size_label = input$font_size)
      svg_content(svg)
    }
  })

  # Preview output
  output$svg_preview <- renderUI({
    svg <- svg_content()
    if (nchar(svg) > 0) {
      HTML(svg)
    } else {
      p("Click 'Generate Taxonomy' to preview your diagram.")
    }
  })

  # Save Taxonomy handler — downloads a zip containing SVG + CSV
  output$save_taxonomy <- downloadHandler(
    filename = function() {
      paste0("ai_taxonomy_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      # Build current categories from inputs
      categories <- build_categories()

      # Generate SVG
      svg <- generate_taxonomy_svg(categories, input$main_title, input$main_subtitle,
                                    font_size_label = input$font_size)
      svg_content(svg)

      # Generate CSV
      csv_df <- generate_taxonomy_csv(categories, input$main_title, input$main_subtitle)

      # Write to temp files
      tmp_dir <- tempdir()
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      svg_file <- file.path(tmp_dir, paste0("ai_taxonomy_", timestamp, ".svg"))
      csv_file <- file.path(tmp_dir, paste0("ai_taxonomy_", timestamp, ".csv"))

      writeLines(svg, svg_file)
      write.csv(csv_df, csv_file, row.names = FALSE)

      # Create zip
      zip(file, files = c(svg_file, csv_file), flags = "-j")
    },
    contentType = "application/zip"
  )

  # Upload CSV handler — load a previously saved taxonomy CSV
  observeEvent(input$upload_csv, {
    req(input$upload_csv)
    tryCatch({
      df <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
      parsed <- parse_taxonomy_csv(df)

      # Update the reactive data store
      cat_data$categories <- parsed$categories
      updateTextInput(session, "main_title", value = parsed$main_title)
      updateTextInput(session, "main_subtitle", value = parsed$main_subtitle)

      # Regenerate preview
      svg <- generate_taxonomy_svg(parsed$categories, parsed$main_title, parsed$main_subtitle,
                                    font_size_label = input$font_size)
      svg_content(svg)

      showNotification("Taxonomy loaded from CSV successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading CSV:", e$message), type = "error")
    })
  })

  # Reset button — restore default data
  observeEvent(input$reset_data, {
    cat_data$categories <- default_categories
    updateTextInput(session, "main_title", value = "AI Usage")
    updateTextInput(session, "main_subtitle", value = "A Taxonomy")
    # Regenerate preview with defaults
    svg <- generate_taxonomy_svg(default_categories, "AI Usage", "A Taxonomy",
                                  font_size_label = input$font_size)
    svg_content(svg)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
