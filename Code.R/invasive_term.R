
# Script to identify the (mis)use of ##invasive 

# Group all the data:
setwd("C:/Users/Propietario/Downloads/INVASIVE")

fil <- list.files(pattern = "save")
data_list <- lapply(fil, function(file) {
  df <- read_excel(file)
  df %>%
    mutate(across(everything(), as.character))
})
combined_data <- bind_rows(data_list)
combined_data <- combined_data[!duplicated(combined_data$`Article Title`), ]
names(combined_data)

desired_columns <- c("Article Title", "Source Title", "Document Type", "Author Keywords",
                     "Abstract", "Publication Year", "WoS Categories", "Research Areas", "DOI")

data_list_filtered <- lapply(data_list, function(df) {
  df %>% dplyr::select(all_of(desired_columns[desired_columns %in% names(df)]))
})

combined_data <- bind_rows(data_list_filtered)
combined_data<- combined_data[!duplicated(combined_data$`Article Title`), ]

unique(combined_data$`Document Type`)
combined_data<- combined_data %>% filter(!`Document Type` %in% c("Meeting Abstract","News Item",
                                                                 "Biographical-Item","Poetry","Expression of Concern"))


unique(combined_data$`WoS Categories`)

combined_data<- combined_data %>% filter(!`WoS Categories` %in% 
   c("Humanities, Multidisciplinary; Multidisciplinary Sciences","Endocrinology & Metabolism; Zoology",
     "Engineering, Civil; Environmental Sciences; Water Resources","Information Science & Library Science; Multidisciplinary Sciences",
     "Developmental Biology; Reproductive Biology; Zoology",
     "Mathematics, Interdisciplinary Applications; Multidisciplinary Sciences",
     "Cell Biology; Microbiology","Anthropology; Environmental Sciences; Environmental Studies; Geography; Geography, Physical",
     "Construction & Building Technology; Environmental Sciences",
     "Chemistry, Multidisciplinary; Environmental Sciences; Nanoscience & Nanotechnology",
     "Immunology; Microbiology",
     "Engineering, Civil; Environmental Sciences; Regional & Urban Planning",
     "Cultural Studies; History & Philosophy Of Science; Zoology",
     "Biochemical Research Methods; Microbiology",
     "Environmental Studies; Geography; Regional & Urban Planning; Urban Studies",
     "Toxicology; Tropical Medicine; Zoology","Anthropology; Archaeology; Zoology",
     "Cell Biology; Zoology","Architecture; Construction & Building Technology; Engineering, Civil; Environmental Studies; Urban Studies",
     "Engineering, Multidisciplinary; Engineering, Industrial; Multidisciplinary Sciences; Social Issues",
     "Engineering, Industrial; Multidisciplinary Sciences","Computer Science, Interdisciplinary Applications; Computer Science, Theory & Methods; Multidisciplinary Sciences","Food Science & Technology; Microbiology",
     "Environmental Sciences; Public, Environmental & Occupational Health; Nuclear Science & Technology; Radiology, Nuclear Medicine & Medical Imaging",
     "Environmental Studies; Social Sciences, Interdisciplinary; Transportation; Urban Studies",
     "Forestry; Materials Science, Paper & Wood; Materials Science, Textiles",
     "Construction & Building Technology; Engineering, Civil; Environmental Sciences; Public, Environmental & Occupational Health",
     "Dentistry, Oral Surgery & Medicine; Microbiology","Engineering, Environmental; Environmental Sciences; Meteorology & Atmospheric Sciences",
     "Architecture; Environmental Studies; Urban Studies","Chemistry, Multidisciplinary; Engineering, Environmental; Environmental Sciences","Engineering, Environmental; Environmental Sciences; Environmental Studies; Regional & Urban Planning",
    "Business; Environmental Studies","Microbiology; Nutrition & Dietetics","Green & Sustainable Science & Technology; Energy & Fuels; Environmental Studies",
    "Engineering, Environmental; Environmental Sciences; Metallurgy & Metallurgical Engineering","Green & Sustainable Science & Technology; Energy & Fuels; Engineering, Civil; Engineering, Geological; Environmental Sciences",
    "Dentistry, Oral Surgery & Medicine; Immunology; Microbiology","Computer Science, Interdisciplinary Applications; Engineering, Environmental; Environmental Sciences; Mathematics, Applied","Chemistry, Multidisciplinary; Environmental Sciences"))


unique(combined_data$`Research Areas`)

combined_data<- combined_data %>% filter(!`Research Areas` %in% c("Neurosciences & Neurology; Zoology","Life Sciences & Biomedicine - Other Topics; Biophysics; Environmental Sciences & Ecology; Radiology, Nuclear Medicine & Medical Imaging","Life Sciences & Biomedicine - Other Topics; Cell Biology","Plant Sciences; Chemistry; Food Science & Technology; Nutrition & Dietetics","Immunology; Science & Technology - Other Topics","Business & Economics; Computer Science; Science & Technology - Other Topics; Social Sciences - Other Topics","Immunology; Zoology",
    "Automation & Control Systems; Agriculture; Forestry; Instruments & Instrumentation; Remote Sensing","Anatomy & Morphology; Zoology","Education & Educational Research; Environmental Sciences & Ecology; Mathematics","Science & Technology - Other Topics; Radiology, Nuclear Medicine & Medical Imaging","Robotics; Science & Technology - Other Topics","Astronomy & Astrophysics; Life Sciences & Biomedicine - Other Topics","Psychology; Science & Technology - Other Topics; Neurosciences & Neurology"))

nrow(combined_data)
str(combined_data$`Article Title`)

unique(combined_data$`WoS Categories`)
unique(combined_data$`Research Areas`)
combined_data <- combined_data[-1,] # remove na
count_words <- function(data, column, keyword) {
  sum(str_detect(data[[column]], regex(keyword, ignore_case = TRUE)))
}

keywords <- c("invasive","alien", "non-native", "exotic","nonnative", "invasive alien",
 "invasive non-native","non-indigenous", "allochthonous","introduced", "naturalised", "naturalized")
keyword_counts <- sapply(keywords, count_words, data = combined_data, column = "Article Title")
keyword_counts

keywords2 <- paste(keywords, collapse = "|")
keyword_data <- combined_data %>% 
  filter(str_detect(`Article Title`, regex(keywords2, ignore_case = TRUE)))


has_keyword <- function(title, keywords) {
  # Create a pattern that matches any of the keywords
  pattern <- paste(keywords, collapse = "|")
  # Check if the title contains any of the keywords
  grepl(pattern, title, ignore.case = TRUE)
}
combined_data$has_keyword <- sapply(combined_data$`Article Title`, has_keyword, keywords = keywords)
papers_without_keywords <- combined_data[!combined_data$has_keyword, ]


## Check invasive titles and abstracts  ----

pattern <- paste(c("invasive","invasive alien","invasive non-native"), collapse = "|")
invasive <- combined_data %>% 
filter(str_detect(`Article Title`, regex(pattern, ignore_case = TRUE)))


all_dataset <- combined_data %>%
  filter(grepl("invasive", `Article Title`, ignore.case = TRUE))
per_all <- nrow(all_dataset) / nrow(combined_data) * 100

first_abstract <- combined_data %>%
  filter(grepl(c("first record|new record"), Abstract, ignore.case = TRUE) )
inv<- first_abstract %>%
  filter(grepl("invasive", `Article Title`, ignore.case = TRUE))
per <- nrow(inv) / nrow(first_abstract) * 100


key_dataset <- keyword_data %>%
  filter(grepl("invasive", `Article Title`, ignore.case = TRUE))
per_all <- nrow(key_dataset) / nrow(keyword_data) * 100
first_abstract <- keyword_data %>%
  filter(grepl(c("first record|new record"), Abstract, ignore.case = TRUE) )
inv<- first_abstract %>%
  filter(grepl("invasive", `Article Title`, ignore.case = TRUE))
per <- nrow(inv) / nrow(first_abstract) * 100


bio_dataset <- combined_data1 %>%
  filter(grepl("invasive", `Article Title`, ignore.case = TRUE))
per_all <- nrow(bio_dataset) / nrow(combined_data1) * 100
first_abstract <- combined_data1 %>%
  filter(grepl(c("first record|new record"), Abstract, ignore.case = TRUE) )
inv<- first_abstract %>%
  filter(grepl("invasive", `Article Title`, ignore.case = TRUE))
per <- nrow(inv) / nrow(first_abstract) * 100

write_xlsx(invasive2, "invasive_use.xlsx")

##### check for journal and some keywords --------

combined_data # all the data
combined_data1 = combined_data  %>% filter(`Source Title` =="BIOINVASIONS RECORDS") # 551 papers
nrow(combined_data1)


df =read_xlsx("invasive_use.xlsx")

zz = combined_data  %>% filter(`Source Title` =="BIOINVASIONS RECORDS")%>% 
  filter(str_detect(Abstract, "invasive")) # 236 abstract

df1 = zz  %>% filter(`Source Title` =="BIOINVASIONS RECORDS")%>% 
  filter(str_detect(`Article Title`, "invasive")) # 236 abstract

df2 <- df %>%
  filter(grepl("first record", Abstract, ignore.case = TRUE)) %>% 
  filter(!`Source Title` =="BIOINVASIONS RECORDS") # 101 in the title



first_record <- combined_data %>%
  filter(grepl("first record", Abstract, ignore.case = TRUE) )


first_record1 = first_record  %>% 
  filter(str_detect(Abstract, "invasive")) # 344 abstract

first_record2 = first_record  %>%
  filter(str_detect(`Article Title`, "invasive"))# 183 abstract


## Check ICAIS  -----
install.packages("pdftools")
library(pdftools)
library(stringr)

ICAIS <- pdf_text("ICAIS-2024.pdf")

pdf_text <- paste(ICAIS, collapse = " ")

titles <- unlist(strsplit(pdf_text, "Abstract"))


if (nchar(titles[1]) == 0) {
  titles <- titles[-1]
}

extract_title <- function(text) {
  match <- str_match(text, "\\s\\d+\\s(.+?)(?=\\s\\d+\\s|\\s[A-Z]|$)")
  if (!is.na(match[2])) {
    return(match[2])
  }
  return(NA)
}

titles1 <- sapply(titles, extract_title) %>% as.data.frame()
titles1$title <- rownames(titles1)
write_xlsx(titles1, "titles1.xlsx")

title <- read_xlsx("titles1.xlsx")


## temporal trend % invasive in title  -----
invasive

invasive$`Publication Year` <- as.numeric(invasive$`Publication Year`)

keywords <- c("invasive","alien", "non-native", "exotic","nonnative", "invasive alien",
              "invasive non-native","non-indigenous", "allochthonous","introduced", "naturalised", "naturalized")


pattern <- paste(keywords, collapse = "|")

all_data <- combined_data %>% 
  filter(str_detect(`Article Title`, regex(pattern, ignore_case = TRUE)))

all_papers <- combined_data %>%
  group_by(`Publication Year`) %>%
  summarize(
    Total = n(),
    Invasive_Count = sum(str_detect(`Article Title`, regex("invasive", ignore_case = TRUE)))) %>%
  mutate(Percentage= (Invasive_Count / Total) * 100) %>% filter(`Publication Year` <2024)

keywords_papers <- all_data %>%  # Keywords
  group_by(`Publication Year`) %>%
  summarize(
    Total = n(),
    Invasive_Count = sum(str_detect(`Article Title`, regex("invasive", ignore_case = TRUE)))) %>%
  mutate(Percentage= (Invasive_Count / Total) * 100) %>% filter(`Publication Year` <2024)

bio_records = combined_data  %>% filter(`Source Title` =="BIOINVASIONS RECORDS") %>% 
  group_by(`Publication Year`) %>%
  summarize(
    Total = n(),
    Invasive_Count = sum(str_detect(`Article Title`, regex("invasive", ignore_case = TRUE)))) %>%
  mutate(Percentage= (Invasive_Count / Total) * 100) %>% filter(`Publication Year` <2024)
  
  
all_papers$`Publication Year` <- as.numeric(all_papers$`Publication Year`)
keywords_papers$`Publication Year` <- as.numeric(keywords_papers$`Publication Year`)
bio_records$`Publication Year` <- as.numeric(bio_records$`Publication Year`)

ggplot(all_papers, aes(x = `Publication Year`, y = Percentage)) +
  geom_line(data = keywords_papers, aes(x = `Publication Year`, y = Percentage), color = "darkorchid2", size = 1.5) +
  geom_line(data = all_papers, aes(x = `Publication Year`, y = Percentage), color = "dodgerblue2", size = 1.5) +
  #geom_line(data = bio_records, aes(x = `Publication Year`, y = Percentage), color = "seagreen2", size = 1.5) +
  #geom_point(data = bio_records, aes(x = `Publication Year`, y = Percentage), color = "seagreen4", size =3) +
  geom_point(data = keywords_papers, aes(x = `Publication Year`, y = Percentage), size = 3, color = "darkorchid4") +
  geom_point(data = all_papers, aes(x = `Publication Year`, y = Percentage), size = 3, color = "dodgerblue4") +
  scale_x_continuous(breaks=c(2000, 2005, 2010,2015, 2020, 2025)) + 
  scale_y_continuous(breaks=c(20,40,60,80)) + ylim(0,80)+ xlim(2000,2025)+
  labs(x = "Year",  y = "Percentage of 'invasive' on titles (%)") +
  theme_bw()  +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"))  


ggplot(all_papers, aes(x = `Publication Year`, y = Percentage)) +
  geom_line(data = keywords_papers, aes(y = Percentage), color = "darkorchid2", size = 2, linetype = "solid") +
  geom_line(data = all_papers, aes(y = Percentage), color = "dodgerblue2", size = 2, linetype = "solid") +
  geom_point(data = keywords_papers, aes(y = Percentage), size = 5, shape = 21, fill = "darkorchid3") +
  geom_point(data = all_papers, aes(y = Percentage), size = 5, shape = 21, fill = "dodgerblue3") +
  
  scale_x_continuous(breaks = seq(2000, 2025, by = 5)) +
  scale_y_continuous(breaks = seq(0, 80, by = 20), labels = percent_format(scale = 1)) +
  coord_cartesian(xlim = c(2000, 2025), ylim = c(0, 80)) +
  labs(x = "Publication year", y = "Percentage of 'invasive' in article titles") +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "#333333"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.caption = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(color = "black")
  )

# panel b
inva_data2$Category <- 'Invasive'
no_inva_data2$Category <- 'Other keywords'
colnames(inva_data2)[10] <- "Citations"
df <- rbind(inva_data2[,c(11,10)], no_inva_data2[,c(11,10)])

ggplot(df, aes(x = Category, y = Citations, fill = Category)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +  # Customize outliers
  scale_fill_manual(values = c("skyblue", "salmon")) +  # Color customization
  labs(
       x = "",
       y = "Number of citations") + ylim(0,50)+
  theme_bw() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    legend.position = "none",  # Hide legend if unnecessary
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(size = 14)
  )


## BIOINVASION RECORD

combined_data1 = combined_data  %>% filter(`Source Title` =="BIOINVASIONS RECORDS") # 551 papers
nrow(combined_data1)

zz = combined_data  %>% filter(`Source Title` =="BIOINVASIONS RECORDS") %>% 
  filter(str_detect(`Article Title`, "invasive")) # 236 abstract

yearly_bio <- zz %>%
  group_by(`Publication Year`) %>%
  summarize(Total = n())

yearly_data2 <- combined_data1 %>%
  group_by(`Publication Year`) %>%
  summarize(Total = n())

yearly_percentage2 <- merge(yearly_bio, yearly_data2, by = "Publication Year",  all.x = TRUE)
yearly_percentage2$Percentage <- (yearly_percentage2$Total.x / yearly_percentage2$Total.y) * 100
yearly_percentage2$`Publication Year` <- as.numeric(yearly_percentage2$`Publication Year`)
yearly_percentage2 = yearly_percentage2 %>% filter(`Publication Year` <2024)

ggplot(yearly_percentage2, aes(x = `Publication Year`, y = Percentage)) +
  geom_line(data = yearly_percentage2, aes(x = `Publication Year`, y = Percentage), size = 1.2) +
  geom_point(data = yearly_percentage2, aes(x = `Publication Year`, y = Percentage), size = 3, color = "black") +
  scale_x_continuous(breaks=c(2000, 2005, 2010,2015, 2020, 2025)) + 
  scale_y_continuous(breaks=c(20,40,60,80)) + ylim(0,80)+ xlim(2000,2025)+
  labs(x = "Year",  y = "Percentage of 'invasive' on titles (%)") +
  theme_bw()  +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"))  



## Check citations  -----
combined_data

pattern <- paste(c("invasive","invasive alien","invasive non-native"), collapse = "|")
invasive <- combined_data %>% 
  filter(str_detect(`Article Title`, regex(pattern, ignore_case = TRUE)))

counter = 1
res = data.frame()
for(i in unique(invasive$`Article Title`)) {
  
  combined_data1 = invasive %>% filter(`Article Title` == i)
  title = combined_data1$`Article Title`[1]
  
  if (!is.null(title)) {
  formatted_title <- gsub(" ", "+", title)
  web = "https://www.researchgate.net/search.Search.html?query=" 
 search = paste0(web,formatted_title,"&type=publication")
 
 page <- rvest::read_html(search)
 text <- rvest::html_text(page)
 citations <- page %>%
   html_nodes(".search-content .search-wrapper .search-container .nova-legacy-o-stack .search-box__result-item [data-testid='searchResultItem'] .nova-legacy-v-entity-item__meta-data .nova-legacy-e-list__item .nova-legacy-v-entity-item__meta-data-item span") %>%
   html_text()

 search_content_children <- page %>%
   html_node(".search__content") %>%
   html_children()
 print(search_content_children)
 
 search_content_inner <- page %>%
   html_node(".search-content") %>%
   html_children()
 
 cites <- sub(".*?(\\d+)\\s+Citation.*", "\\1", text)
 
 res  = rbind(res, data.frame("DOI" = doi,
                              "title" =combined_data1$`Article Title`,
                              "Citation" = cites))
  }
  cat(counter, "/", unique(combined_data$DOI))
  counter = counter + 1
  Sys.sleep(runif(1, min = 60, max = 120))  
}


# Now lets check the analysis of the cites and statistics:

def <- data.frame(
  Year = c(1986, 1993, 1995, 1999, 2000,2000, 2004,2004, 2007, 2008, 2011,2011,2011,2011,2011,2011, 2012,2012,2014,2018,2020,2023,2024,2024),
  Def = c("Usher, 1986", "CBD, 1993", "Pyšek et al., 1995", "Executive Order 13112", "Richardson et al., 2000", "Davis & Thompson, 2000",
                 "Colautti & MacIsaac, 2004","Richardson & Pyšek, 2004", "Ricciardi & Cohen, 2007", "Beck et al., 2008", "Blackburn et al., 2011",
          "Gibson et al., 2011","Hulme & Weser 2011","Oreska & Aldridge 2011","D'Antonio et al., 2011", "Eichiner 2012", "Bauer, 2012",
                 "Mooney & Hobbs, 2011", "Piraino et al., 2014", "Guy-Haim et al., 2018","Essl et al., 2020",
          "IPBES, 2023", "Soto et al., 2024", "Oficialdegui et al., 2024") )
head(def)

def2 <- def%>%  group_by(Year) %>%
  summarise(NumDefinitions = n(), .groups = 'drop') %>%
  mutate(CumulativeDefinitions = cumsum(NumDefinitions))

def3 <- def %>% left_join(def2, by="Year")
def3 <- def3[!duplicated(def3$Year), ]



ggplot(def3, aes(x = Year, y = CumulativeDefinitions)) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "grey80", linetype = "dashed") +

  geom_line(aes(color = CumulativeDefinitions), size = 2, alpha = 0.8) +
  scale_color_gradientn(colors = colorRampPalette(brewer.pal(11, "YlOrRd"))(50)) +  # Using "Spectral" palette
  geom_point(aes(fill = CumulativeDefinitions), size = 5, shape = 21, color = "black", stroke = 1.5) +
  scale_fill_gradientn(colors = colorRampPalette(brewer.pal(11, "YlOrRd"))(50)) +
  
  labs( x = "Year", y = "Cumulative number of definitions") +

  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = "right",
    legend.title = element_blank(),
    panel.grid.major = element_line(color = "gray", linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

#invasive_cites <- inv
#no_invasive_cites <- first_abstract

inva_cites <- read_xlsx("new_invasive_cites.xlsx")
no_inva_cites <- read_xlsx("no-invasives.xlsx")

inva_data <- keyword_data %>%  filter(grepl("invasive", `Article Title`, ignore.case = TRUE))
nrow(inva_data)
inva_data2<- inva_data %>% left_join(inva_cites[,c(2,4)], by ="Article Title")

no_inva_data <- keyword_data %>%  filter(!grepl("invasive", `Article Title`, ignore.case = TRUE))
nrow(no_inva_data)
no_inva_data2<- no_inva_data %>% left_join(no_inva_cites[,c(2,4)], by ="Article Title")
no_inva_data2 <- no_inva_data2 %>% filter(!`WoS Categories` %in% remove )

unique(inva_data2$`WoS Categories`)
remove <- c("Anthropology; Environmental Studies; Sociology","Microbiology; Virology","Green & Sustainable Science & Technology; Environmental Sciences; Environmental Studies",
            "Chemistry, Inorganic & Nuclear; Environmental Sciences","Chemistry, Analytical; Environmental Sciences","Microbiology; Parasitology; Virology","Economics; Energy & Fuels; Environmental Sciences; Environmental Studies",
            "Automation & Control Systems; Agricultural Engineering; Forestry","Multidisciplinary Sciences; Otorhinolaryngology","Engineering, Environmental; Engineering, Civil; Environmental Sciences; Statistics & Probability; Water Resources",
            "Computer Science, Information Systems; Computer Science, Interdisciplinary Applications; Multidisciplinary Sciences",
            "Computer Science, Interdisciplinary Applications; Engineering, Environmental; Environmental Sciences; Water Resources",
            "Green & Sustainable Science & Technology; Environmental Sciences","Chemistry, Multidisciplinary; Energy & Fuels; Engineering, Chemical; Environmental Sciences",
            "Green & Sustainable Science & Technology; Engineering, Environmental; Environmental Sciences",
            "Biochemical Research Methods; Plant Sciences; Chemistry, Analytical", "Anthropology; Archaeology; Multidisciplinary Sciences",
            "Green & Sustainable Science & Technology; Environmental Sciences; Marine & Freshwater Biology; Water Resources",
            "Anatomy & Morphology; Biology; Microscopy","Computer Science, Interdisciplinary Applications; Engineering, Environmental; Environmental Studies; Geography; Operations Research & Management Science; Regional & Urban Planning")

inva_data2 <- inva_data2 %>% filter(!`WoS Categories` %in% remove )
inva_data2 <- inva_data2 %>% filter(`Number of citations` > 0 )
inva_data2$`Number of citations` <- as.numeric(inva_data2$`Number of citations`)
inva_data2 <- inva_data2 %>% filter(!`Number of citations` %in% c("0/error","Error 03", "error") )

mean(inva_data2$`Number of citations`)
median(inva_data2$`Number of citations`)

no_inva_data2 <- no_inva_data2 %>% filter(Citations > 0 )
no_inva_data2$Citations <- as.numeric(no_inva_data2$Citations)
no_inva_data2 <- no_inva_data2 %>% filter(!Citations %in% c("0/error","Error 03", "error") )

mean(no_inva_data2$Citations)
median(no_inva_data2$Citations)

ks.test(as.numeric(inva_data2$`Number of citations`),  'pnorm')
ks.test(as.numeric(no_inva_data2$Citations), 'pnorm')

test <- wilcox.test(as.numeric(inva_data2$`Number of citations`), as.numeric(no_inva_data2$Citations))
# W = 107683783, p-value = 0.005636


# Corrected datasets ----

df1 <- read_xlsx("./corrected_dataset/new_invasive_cites.xlsx")
df2 <- read_xlsx("./corrected_dataset/no_invasive_definitivo.xlsx")

head(df1)

df1$type ="Invasive"
df2$type ="No Invasive"

df1 <- df1[,c(2,4,9,10,13)]
colnames(df2)[4] <- "Number of citations"
df2 <- df2[,c(names(df1))]

df <- rbind(df1,df2)
df <- df  %>% # filter(`Number of citations` > 0 ) %>%  
  filter(!`Number of citations` %in% c("0/error","Error 03", "error") )%>%
  filter(!`WoS Categories` %in% remove )

df %>% group_by(type) %>% summarise(Media= mean(as.numeric(`Number of citations`), na.rm=T),
                                    mediana =median(as.numeric(`Number of citations`), na.rm=T))


head(df)

m1 <- glm(as.numeric(`Number of citations`) ~ type*`Publication Year`, data = df, family = "poisson")
summary(m1)
with(summary(m1), 1 - deviance/null.deviance)

m2 <- glm.nb(as.numeric(`Number of citations`) ~ as.factor(type)*`Publication Year`, 
             data = df)
summary(m2)
with(summary(m2), 1 - deviance/null.deviance)

AIC(m1,m2)

emm <- emmeans(m2, specs = ~ type)
#emm2 <- emmeans(m2, specs = pairwise ~ type | `Publication Year`)
emm2 <- pairs(emm2)

performance::check_overdispersion(m2)
performance::check_zeroinflation(m2)
performance::model_performance(m2)

plot(fitted(m2), residuals(m2, type = "pearson"),
     xlab = "Fitted Values", ylab = "Pearson Residuals")
abline(h = 0, col = "red")
