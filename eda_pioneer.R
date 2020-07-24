# library -----------------------------------------------------------------

library(rvest)
library(xml2)
library(curl)
library(tidyverse)
library(janitor)
url <- 'https://history.churchofjesuschrist.org/overlandtravel/companies'
page <- read_html(url)

# company info --------------------

df <-
  as.data.frame(page %>% html_table()) %>%
  row_to_names(row_number = 1) %>%
  rename(num_of_pioneers = `number of pioneers`, dep_date = `departure date`, arriv_date = `arrival date`, company_name = name) %>%
  mutate(num_of_pioneers = as.integer(num_of_pioneers)) %>%
  mutate(dep_year = sub("^.*([0-9]{4}).*", "\\1", dep_date)) %>%
  mutate(arriv_year = sub("^.*([0-9]{4}).*", "\\1", arriv_date)) %>%
  mutate(dep_year = ifelse(company_name == 'Unknown Companies (1847-1868)', '1847', dep_year)) %>%
  mutate(dep_month = ifelse(grepl('June',dep_date),'June',
                            ifelse(grepl('July',dep_date),'July',
                                   ifelse(grepl('August',dep_date),'August',
                                          ifelse(grepl('September',dep_date),'September',
                                                 ifelse(grepl('October',dep_date),'October',
                                                        ifelse(grepl('November',dep_date),'November',
                                                               ifelse(grepl('May',dep_date),'May',
                                                                      ifelse(grepl('April',dep_date),'April',
                                                                             ifelse(grepl('March',dep_date),'March',
                                                                                    ifelse(grepl('December',dep_date),'December',
                                                                                           ifelse(grepl('February',dep_date),'February', NA)))))))))))) %>%
  mutate(arriv_month = ifelse(grepl('June',arriv_date),'June',
                              ifelse(grepl('July',arriv_date),'July',
                                     ifelse(grepl('August',arriv_date),'August',
                                            ifelse(grepl('September',arriv_date),'September',
                                                   ifelse(grepl('October',arriv_date),'October',
                                                          ifelse(grepl('November',arriv_date),'November',
                                                                 ifelse(grepl('May',arriv_date),'May',
                                                                        ifelse(grepl('January',arriv_date),'January',
                                                                               ifelse(grepl('April',arriv_date),'April',
                                                                                      ifelse(grepl('March',arriv_date),'March',
                                                                                             ifelse(grepl('December',arriv_date),'December',
                                                                                                    ifelse(grepl('February',arriv_date),'February', NA)))))))))))))

links <- as.data.frame(page %>%
                         html_nodes("table") %>%
                         html_nodes("tr") %>%
                         html_nodes("a") %>%
                         html_attr("href")) %>%
  rename(links = `page %>% html_nodes("table") %>% html_nodes("tr") %>% html_nodes("a") %>% html_attr("href")`) %>%
  mutate(link = paste0('https://history.churchofjesuschrist.org')) %>%
  mutate(url = paste0(link, links))

df$link <- links$url

df$dep_year <- sub("^.*([0-9]{4}).*", "\\1", df$dep_date)

# scrape company info -----------------------------------------------------

  webpages <- apply(df, 1, function(x){
    company <- read_html(x['link']) %>% html_nodes(xpath = "/html/body/main/section/h1") %>% html_text()
    
    tt <- read_html(x['link']) %>% 
      html_nodes("[class = 'info-block__item']") %>% 
      html_text() %>% 
      as.data.frame() %>% 
      rename(info = '.') %>% 
      separate(info, c('info', 'support'), "\n") 
    
    tt$support <- str_replace_all(tt$support,"[\\r\\n\\t]+", "") 
    tempdf<-data.frame(company, tt)
  })

comp_info <- do.call(rbind, webpages)  

comp_info <- comp_info %>% 
  mutate(info = ifelse(info == 'Captains', 'Captain', info)) %>% 
  pivot_wider(names_from = info, values_from = support) %>%
  rename(company_name = company)

  
# pioneer information -----------------------------------------------------

  # Splitting up what I am scrapping. When it is done all at once it errors.
  # Filtering out num_of_pioneers with NA or only 1. 
dfs <- df %>% filter(!is.na(num_of_pioneers), num_of_pioneers != 1)
  
  # Run scrape
df1 <- dfs %>% slice(1:100)
webpages1 <- apply(df1, 1, function(x){
  web_read <- read_html(x['link'])

  comp_df <- web_read %>% html_nodes("[class='page-title content-page-title']") %>% html_text()
  
  pioneer_df <- as.data.frame(web_read %>% html_table()) %>%
    row_to_names(row_number = 1) %>% 
    mutate(company_name = comp_df)
})
pio_df1 <- do.call(rbind, webpages1)

df2 <- dfs %>% slice(101:200)
webpages2 <- apply(df2, 1, function(x){
  web_read <- read_html(x['link'])
  
  comp_df <- web_read %>% html_nodes("[class='page-title content-page-title']") %>% html_text()
  
  pioneer_df <- as.data.frame(web_read %>% html_table()) %>%
    row_to_names(row_number = 1) %>% 
    mutate(company_name = comp_df)
})
pio_df2 <- do.call(rbind, webpages2)

df3 <- dfs %>% slice(201:300)
webpages3 <- apply(df3, 1, function(x){
  web_read <- read_html(x['link'])
  
  comp_df <- web_read %>% html_nodes("[class='page-title content-page-title']") %>% html_text()
  
  pioneer_df <- as.data.frame(web_read %>% html_table()) %>%
    row_to_names(row_number = 1) %>% 
    mutate(company_name = comp_df)
})
pio_df3 <- do.call(rbind, webpages3)

df4 <- dfs %>% slice(301:380)
webpages4 <- apply(df4, 1, function(x){
  web_read <- read_html(x['link'])
  
  comp_df <- web_read %>% html_nodes("[class='page-title content-page-title']") %>% html_text()
  
  pioneer_df <- as.data.frame(web_read %>% html_table()) %>% 
    row_to_names(row_number = 1) %>% 
    mutate(company_name = comp_df)
})
pio_df4 <- do.call(rbind, webpages4)

pio_df <- rbind(pio_df1, pio_df2, pio_df3, pio_df4)


pio_df <- pio_df  %>% 
  mutate(birth_year = sub("^.*([0-9]{4}).*", "\\1", birthdate)) %>%
  mutate(death_year = sub("^.*([0-9]{4}).*", "\\1", deathdate)) %>%
  mutate(birth_month = ifelse(grepl('June',birthdate),'June',
                            ifelse(grepl('July',birthdate),'July',
                                   ifelse(grepl('August',birthdate),'August',
                                          ifelse(grepl('September',birthdate),'September',
                                                 ifelse(grepl('October',birthdate),'October',
                                                        ifelse(grepl('November',birthdate),'November',
                                                               ifelse(grepl('Janruary',birthdate),'Janruary',
                                                               ifelse(grepl('May',birthdate),'May',
                                                                      ifelse(grepl('April',birthdate),'April',
                                                                             ifelse(grepl('March',birthdate),'March',
                                                                                    ifelse(grepl('December',birthdate),'December',
                                                                                           ifelse(grepl('February',birthdate),'February', NA))))))))))))) %>%
  mutate(death_month = ifelse(grepl('June',deathdate),'June',
                              ifelse(grepl('July',deathdate),'July',
                                     ifelse(grepl('August',deathdate),'August',
                                            ifelse(grepl('September',deathdate),'September',
                                                   ifelse(grepl('October',deathdate),'October',
                                                          ifelse(grepl('November',deathdate),'November',
                                                                 ifelse(grepl('May',deathdate),'May',
                                                                        ifelse(grepl('January',deathdate),'January',
                                                                               ifelse(grepl('April',deathdate),'April',
                                                                                      ifelse(grepl('March',deathdate),'March',
                                                                                             ifelse(grepl('December',deathdate),'December',
                                                                                                    ifelse(grepl('February',deathdate),'February', NA)))))))))))))




# df, comp_info, pio_df combine -------------------------------------------


dfs <- pio_df %>% left_join(df) %>% left_join(comp_info)

# Exploration -------------------------------------------------------------------

  # Where pioneers departed from per year
dfs %>% filter(!is.na(`Departed From`), `Departed From` != 'Other', `Departed From` != 'Unknown', num_of_pioneers > 200) %>% 
  ggplot(aes(`Departed From`, arriv_year, fill = `Departed From`, size = num_of_pioneers)) +
  geom_point(alpha = 0.4, show.legend = FALSE) +
  labs(x = NULL, y = "Year") + 
  coord_flip() +
  labs(
    x = "Departed Location", y = "Year",
    title = "Most departed loction of travel",
    caption = "Pioneer Database from https://history.churchofjesuschrist.org/overlandtravel/"
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  # Number of pioneer arrivals per year
dfs %>% 
  ggplot(aes(arriv_year)) +
  geom_histogram(fill = 'midnightblue', stat = 'count', alpha = 0.7) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = 'Number of Pioneers arriving to SLC per year',
    y = "Nu of Pioneers",
    x = 'Year'
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  # Boxplot of mean age
dfs %>%
  ggplot(aes(age, arriv_year, fill = age)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  labs(x = NULL, y = "Year") +
  labs(
    x = "Mean age", y = "Year",
    title = "Mean age and year of arrival",
    caption = "Pioneer Database from https://history.churchofjesuschrist.org/overlandtravel/"
  )


  # Mean age per year
dfs %>%
  group_by(arriv_year) %>%
  summarise(age = mean(age, na.rm = TRUE)) %>%
  ggplot(aes(arriv_year, age, group = 1)) +
  geom_line(alpha = 0.6, size = 1.5) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(20, NA)) +
  labs(x = NULL, y = "Mean age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(
    x = "Mean year of arrival", y = "Mean age",
    title = "Mean age per year of arrival",
    caption = "Pioneer Database from https://history.churchofjesuschrist.org/overlandtravel/"
  )



  # Most common first name per year
library(ggrepel)
dfs %>%
  mutate(arriv_year = as.integer(arriv_year)) %>% 
  separate(name, c('last_name', 'first_name')) %>% 
  filter(first_name != '', first_name != 'A', first_name != 'Sister', first_name != 'J', first_name != 'or', first_name != 'Mr', first_name != 'Mrs') %>%
  group_by(first_name) %>%
  summarise(n = n(),
    age = mean(age, na.rm = TRUE),
    arriv_year = mean(arriv_year, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(-n) %>%
  filter(n > 30) %>%
  ggplot(aes(arriv_year, age)) +
  geom_text_repel(aes(label = first_name), size = 3) +
  geom_point(aes(size = n), color = "midnightblue", alpha = 0.7) +
  labs(
    x = "Mean year of arrival", y = "Mean age",
    size = "Number of people",
    title = "Age and year of arrival for most common names of traveled pioneers",
    caption = "Pioneer Database from https://history.churchofjesuschrist.org/overlandtravel/"
  )
  
  
  
  # eda of deaths per year -------------------------------------------------------------
dfs %>% mutate(age = ifelse(age == 'Infant', 1, age),
                  age = as.integer(age))%>%
  filter(dep_year == death_year,
              !is.na(death_month),
              !is.na(arriv_month)) %>%
  mutate(death_month = 
ifelse(death_month == 'October',10,
ifelse(death_month == 'September',9,
ifelse(death_month == 'November',11,
ifelse(death_month == 'August',8,
ifelse(death_month == 'July',7,
ifelse(death_month == 'April',4,
ifelse(death_month == 'June',6,
ifelse(death_month == 'December',12,
ifelse(death_month == 'May',5,
ifelse(death_month == 'Janruary', 1, death_month))))))))))) %>%
  mutate(arriv_month = 
ifelse(arriv_month == 'October',10,
ifelse(arriv_month == 'September',9,
ifelse(arriv_month == 'November',11,
ifelse(arriv_month == 'August',8,
ifelse(arriv_month == 'July',7,
ifelse(arriv_month == 'April',4,
ifelse(arriv_month == 'June',6,
ifelse(arriv_month == 'December',12,
ifelse(arriv_month == 'May',5,
ifelse(arriv_month == 'Janruary', 1, arriv_month))))))))))) %>% 
  mutate(death_month = as.integer(death_month),
         arriv_month = as.integer(arriv_month)) %>% 
  filter(death_month <= arriv_month) %>%
  ggplot(aes(age, arriv_year, fill = age)) +
  geom_boxplot(alpha = 0.4, show.legend = FALSE) +
  labs(x = NULL, y = "Year") +
  labs(
    x = "Mean death age", y = "Year",
    title = "Mean death age per year",
    caption = "Pioneer Database from https://history.churchofjesuschrist.org/overlandtravel/"
  ) 
