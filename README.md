# 🌾 Agricultural Crop Insight Dashboard

An interactive **R Shiny dashboard** designed to analyze crop production data, visualize trends, forecast future production, and assist farmers with intelligent recommendations.

---

## 🚀 Features

### 📊 Data Analysis & Visualization

* Interactive production trends over years
* State-wise and crop-wise analysis
* Distribution plots and clustering

### 📈 KPI Dashboard

* Total production (in Million Tonnes)
* Total records
* Unique crops and states
* Data quality indicator
* Year-over-year production trend

### 🔮 Forecasting

* 3-year production forecast using Linear Regression
* Confidence intervals for predictions

### 🌱 Farmer Assistant

* Seasonal crop recommendations (Kharif, Rabi, Summer)
* Best farming practices guidance

### 🐛 Pest Prediction

* Crop + season based pest risk prediction
* Suggested pest management solutions
* Integrated Pest Management (IPM) guidance

### 📥 Export Options

* Download filtered dataset (CSV)
* Export charts as PNG

---

## 🛠️ Tech Stack

* R
* Shiny
* ggplot2
* plotly
* dplyr
* readr

---

## 📦 Requirements

Install required packages before running the project:

install.packages(c(
  "shiny",
  "ggplot2",
  "dplyr",
  "plotly",
  "readr",
  "scales"
))

---
## 📂 Project Structure

```
crop-insight-dashboard/
│── app.R  (or ui.R + server.R)
│── data/
│── README.md
```

---

## 📊 Dataset Requirements

The uploaded dataset must contain the following columns:

* State
* Crop
* Season
* Crop_Year
* Production


---
## ▶️ How to Run the Project

1. Open the project in RStudio

2. Install required packages:
   install.packages(c("shiny","ggplot2","dplyr","plotly","readr","scales"))

3. Run the app:
   shinyApp(ui, server)

---

## 💡 Use Cases

* Agricultural data analysis
* Crop production monitoring
* Decision support for farmers
* Academic & research projects

---

## 👩‍💻 Author

K M Mythri Gowda

---

## 🌟 Future Enhancements

* Machine learning-based prediction models
* Real-time weather integration
* Mobile-friendly UI
* Multi-language support

---

## 📌 License

This project is for academic and learning purposes.
