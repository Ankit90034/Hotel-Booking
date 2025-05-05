# Load the necessary librarries
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(corrplot)

# Load dataset
hotel_data <- read.csv("C:/Users/DELL/Downloads/hotel_bookings.csv", stringsAsFactors = FALSE)

# View the structure of the data
str(hotel_data)
summary(hotel_data)
glimpse(hotel_data)
colnames(hotel_data)

# Data cleaning
# Check for missing values
colSums(is.na(hotel_data))

# Remove the rows with missing data
hotel_data <- na.omit(hotel_data)

# Convert month names to numbers
hotel_data$arrival_date_month <- match(tolower(hotel_data$arrival_date_month), tolower(month.name))

# Combine year, month, and day into a single date
hotel_data$arrival_date <- make_date(
  year = hotel_data$arrival_date_year,
  month = hotel_data$arrival_date_month,
  day = hotel_data$arrival_date_day_of_month
)


# Convert date columns
# arrival date column is converted into format '2015-07-01'
hotel_data$arrival_date <- with(hotel_data, paste(arrival_date_year, arrival_date_month, arrival_date_day_of_month, sep = "-"))
hotel_data$arrival_date <- ymd(hotel_data$arrival_date)

# Remove the unnecessary columns
# Because some columns are used to create the arival_date columns so after that those 
# columns are removed
hotel_data <- hotel_data %>%
  select(-c(arrival_date_year, arrival_date_month, arrival_date_day_of_month, agent, company,
            reservation_status_date))

# Confirm changes
glimpse(hotel_data)

# Check the unique values in some categorical columns
# Check unique values in some categorical columns
unique(hotel_data$meal)
unique(hotel_data$market_segment)
unique(hotel_data$distribution_channel)

# Fix meal type 'Undefined'
hotel_data$meal[hotel_data$meal == "Undefined"] <- "SC"

# Fix inconsistent customer type and  market_segment
hotel_data$market_segment <- trimws(tolower(hotel_data$market_segment))
hotel_data$distribution_channel <- trimws(tolower(hotel_data$distribution_channel))
hotel_data$distribution_channel[hotel_data$distribution_channel == "undefined"] <- "unknown"

# Create some new features from existing columns
# Total guests
hotel_data$total_guests <- hotel_data$adults + hotel_data$children + hotel_data$babies

# Length of stay
hotel_data$total_nights <- hotel_data$stays_in_weekend_nights + hotel_data$stays_in_week_nights

# Average revenue per stay
hotel_data$revenue_per_stay <- hotel_data$adr * hotel_data$total_nights

# Convert categorical variables to factor
hotel_data <- hotel_data %>%
  mutate(across(c(hotel, meal, market_segment, distribution_channel,
                  customer_type, deposit_type, reserved_room_type,
                  assigned_room_type, reservation_status), as.factor))

# Remove invalid or impossible values
# Remove bookings with zero guests
hotel_data <- hotel_data %>% filter(total_guests > 0)

# Remove negative or unrealistic 'adr'
hotel_data <- hotel_data %>% filter(adr >= 0)

# Give columns meaningful names

colnames(hotel_data) <- c(
  "Hotel", "IsCanceled", "LeadTime","ArrivalDateWeekNumber", "StaysInWeekendNights", "StaysInWeekNights",
  "Adults", "Children", "Babies", "Meal", "Country", "MarketSegment",
  "DistributionChannel", "IsRepeatedGuest", "PreviousCancellations",
  "PreviousBookingsNotCanceled", "ReservedRoomType", "AssignedRoomType",
  "BookingChanges", "DepositType", "DaysInWaitingList",
  "CustomerType", "ADR", "RequiredCarParkingSpaces", "TotalOfSpecialRequests",
  "ReservationStatus", "ArrivalDate", "TotalGuests", "TotalNights", "RevenuePerStay"
)


# Confirm changes
colnames(hotel_data)


# Now perform data analysis
# Booking distribution by hotel
hotel_data %>%
  count(Hotel) %>%
  ggplot(aes(x = Hotel, y = n, fill = Hotel)) +
  geom_col() +
  labs(title = "Number of Bookings by Hotel", y = "Bookings")

# Average Daily Rate by Hotel type
hotel_data %>%
  group_by(Hotel) %>%
  summarise(avg_adr = mean(ADR, na.rm = TRUE)) %>%
  ggplot(aes(x = Hotel, y = avg_adr, fill = Hotel)) +
  geom_col() +
  labs(title = "Average Daily Rate by Hotel", y = "ADR ($)")

# Booking trends over time
hotel_data %>%
  group_by(ArrivalDate) %>%
  summarise(bookings = n()) %>%
  ggplot(aes(x = ArrivalDate, y = bookings)) +
  geom_line(color = "skyblue") +
  labs(title = "Booking Trends Over Time", x = "Date", y = "Bookings")

# Cancellation rate
hotel_data %>%
  count(IsCanceled) %>%
  mutate(rate = n / sum(n)) %>%
  ggplot(aes(x = factor(IsCanceled), y = rate, fill = factor(IsCanceled))) +
  geom_col() +
  scale_fill_manual(values = c("steelblue", "red"), labels = c("Not Canceled", "Canceled")) +
  labs(title = "Cancellation Rate", x = "Canceled", y = "Rate")

# Cancellation rate by segment
hotel_data %>%
  group_by(MarketSegment) %>%
  summarise(
    CancellationRate = mean(IsCanceled),
    Bookings = n()
  ) %>%
  arrange(desc(CancellationRate))

ggplot(hotel_data, aes(x = MarketSegment, fill = as.factor(IsCanceled))) +
  geom_bar(position = "fill") +
  ylab("Proportion") + xlab("Market Segment") +
  ggtitle("Cancellation Rate by Market Segment") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "tomato"), name = "Canceled") +
  theme(axis.text.x = element_text(angle = 45))


# Average revenue per customer type
hotel_data %>%
  group_by(CustomerType) %>%
  summarise(
    AvgADR = mean(ADR, na.rm = TRUE),
    AvgNights = mean(TotalNights, na.rm = TRUE),
    AvgRevenue = mean(RevenuePerStay, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(AvgRevenue))

# Does Longer Stay leads to higher revenue

ggplot(hotel_data, aes(x = TotalNights, y = RevenuePerStay)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Revenue vs. Length of Stay", x = "Total Nights", y = "Revenue Per Stay") +
  theme_minimal()


# What proportion of customers are transient, group etc.
customer_dist <- hotel_data %>%
  group_by(CustomerType) %>%
  summarise(Count = n())

ggplot(customer_dist, aes(x = "", y = Count, fill = CustomerType)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Customer Type Distribution") +
  theme_void()




  




