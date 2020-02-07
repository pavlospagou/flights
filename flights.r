#Εργασία 1
########################################################3
#Κάτω από κάθε ερώτηση να τοποθετήσετε το κώδικα-απάντηση της αντίστοιχης ερώτησης
#Μπορείτε για κάθε απάντηση να χρησιμοποιήσετε οποιοδήποτε μοτίβο κώδικα έχετε διδαχθεί
#An den emfanizontai sosta ta ellinika epilegetai apo to menu tools->global options->code->saving->default code encoding->utf-8
#epeita epilegetai apply kleinete to arxeio kai to ksanaanoigete

#Να υπολογίσετε και να εμφανίσετε τις απαντήσεις για κάθε ένα από τα παρακάτω ερωτήματα

#prepare workspace. trexte to programma apo edw.

rm(list = ls())
library(dplyr)


#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset

apply (DelayedFlights, 2, function(x){sum(as.numeric(is.na(x)))})#calculate na at columns
                 
#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων

DelayedFlights %>% group_by(DayofMonth, DayOfWeek) %>% 
  summarise(number_delays = length(DepDelay)) %>% 
  ungroup() %>% arrange(-number_delays) %>% slice(1)

#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων για καθέναν από τους θερινούς μήνες του 2008

DelayedFlights %>% filter(Month %in% 6:8 & Year == 2008) %>% 
  group_by(Year, Month, DayofMonth) %>% 
  summarise(average_delay = mean(DepDelay, na.rm = TRUE))

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β

DelayedFlights %>% filter(CancellationCode == 'B') %>% 
  group_by(UniqueCarrier) %>% summarise(cancelations_B_number = n()) %>% 
  ungroup() %>%
  arrange(-cancelations_B_number) %>% slice(1)

#Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων

DelayedFlights %>% group_by(FlightNum) %>% summarise(number_delays = n()) %>%
  ungroup() %>% arrange(-number_delays) %>% slice(1)

#Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση προορισμού με τις περισσότερες καθυστερήσεις

DelayedFlights %>%  filter(Distance %in% max(Distance, na.rm = TRUE)) %>% 
  group_by(Dest) %>% summarise(number_Depdelays = length(DepDelay)) %>% 
  ungroup() %>% arrange(-number_Depdelays) %>% slice(1)

#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)

DelayedFlights %>% filter(Cancelled == 0) %>% group_by(Dest) %>% 
  summarise(longest_del = max(DepDelay, na.rm = TRUE)) %>% ungroup() %>% 
  arrange(-longest_del) %>% slice(1)

#Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών

DelayedFlights %>% group_by(UniqueCarrier) %>% 
  summarise(longest_LateAircraft_Delay = max(LateAircraftDelay, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(-longest_LateAircraft_Delay) %>% slice(1)

#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα

DelayedFlights %>% filter(CancellationCode == "A" & DayofMonth == 13) %>% 
  group_by(Month) %>% summarise(CancelationsA_number = n())

#Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008

DelayedFlights %>% filter(Year == '2008', Month == '4', 
                          DayofMonth %in% seq(10, 23) ) %>% 
  summarise(avg_flight_delay = mean(DepDelay, na.rm = TRUE))

#Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00

DelayedFlights %>% 
  filter(SecurityDelay == 1 & DepTime %in% seq(600, 1400)) %>% 
  group_by(Month) %>% summarise(longest_delay = max(DepDelay, na.rm = TRUE)) %>%
  ungroup() %>% arrange(-longest_delay) %>% slice(1)

#Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του Νοεμβρίου του 2008 την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της

DelayedFlights %>% filter(Year == 2008 & Month == 11 & 
                            DayofMonth < 11) %>% 
  group_by(FlightNum) %>% summarize(max_expected_arrival = max(CRSArrTime)) %>% 
  ungroup() %>% arrange(-max_expected_arrival) %>% slice(1)

#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης) είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις με καθυστέρηση(αναχωρίσεων) μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς

DelayedFlights %>% filter(Year == '2008' & Month == 8 & 
                            DayofMonth  %in% seq(11, 20) & 
                            CarrierDelay == 1 & DepDelay > 30) %>% 
  group_by(Dest) %>% 
  summarise(most_delayed_flight = max(DepDelay, na.rm = TRUE)) %>% ungroup() %>%
  arrange(-most_delayed_flight) %>% slice(1)

#Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε

DelayedFlights %>% filter(Diverted == 1 & Cancelled == 0 & !is.na(ActualElapsedTime)) 

#Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης

DelayedFlights %>% group_by(Month) %>% 
  summarise(sd_delays = sd((ActualElapsedTime - CRSElapsedTime), 
                           na.rm = TRUE)) %>% ungroup() %>%
  arrange(-sd_delays) %>% slice(1)




