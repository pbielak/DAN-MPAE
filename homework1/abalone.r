# Abalone dataset analysis
source('utils.r')

abalone_data <- read.csv('data.txt')

####### Liczba osobników w zależności od płci
make_barplot_with_labels(table(abalone_data$Sex),
                         title_str = "Liczba osobników w zależności od płci",
                         ylab = 'Liczba osobników',
                         xlab = 'Płeć',
                         x_names = c("Żeńska", "Nieokreślona", "Męska"))

###### Histogramy wartości atrybutów numerycznych
make_histograms(abalone_data)

###### Barplot liczby osobników o zadanym wieku (Rings) w zależności od płci
x11()
age_sex <- table(abalone_data$Sex, abalone_data$Rings)
barplot(age_sex,
        beside = TRUE,
        col = c("purple", "grey", "blue"),
        xlab = "Rings (~ wiek)",
        ylab = "Liczba osobników",
        main = "Liczba osobników o określonej płci w zależności od wieku")
legend(x = "right",
       legend = c("Żeńskie", "Nieokreślone", "Męskie"),
       col = c("purple", "grey", "blue"),
       lwd = 2)


###### Box-ploty dla atrybutów
make_boxplots(abalone_data)

