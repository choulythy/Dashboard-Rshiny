## 📊 Interactive Dashboards (RShiny)

Explore dynamic dashboards built using **RShiny**, combining data preprocessing, user-friendly interfaces, and visual storytelling.

---

### 🧼 Dashboard 1: Website Usage & Business Insights

An interactive dashboard based on survey data analyzing how users engage with websites, their business types, and likelihood of recommending services.

![GoDaddy Dashboard Screenshot](Dashboard-Rshiny/Screenshot%202025-04-07%20at%204.02.50%20in%20the%20afternoon.png)

**🔍 Key Steps:**
- Imported and merged two Excel sheets (survey data + coded value descriptions)
- Cleaned and filtered responses (handled missing values and labels)
- Generated `df_filtered` for plotting and analysis

**📊 Dashboard Components:**
1. **Usage Purpose Distribution (Table)**  
   → 74.5% of respondents use websites for *commercial* purposes.

2. **Visitor Count by User Category (Bar Chart)**  
   → Commercial users attract the most visitors.

3. **Business Size vs. Website Importance (Bar Chart)**  
   → Large businesses rate websites as "very important."

4. **Income Sources by Business Category (Stacked % Bar Chart)**  
   → High recommendation scores correlate with income-generating sites.

5. **Hosting Duration vs. Recommendation Likelihood (Bar Chart)**  
   → Users with 5+ years of hosting show stronger loyalty.

> 📌 *Showcases preprocessing, merging, recoding, and Shiny dashboard interactivity.*

---

### 🍷 Dashboard 2: Spanish Wine Explorer

A powerful tool to interactively explore **Spanish wine data**, uncovering insights about price, region, and wine type popularity.

**🔧 Features:**
- **Cutoff slider** to filter by wine price quantiles  
- **Type limiter** to show top wine varieties  
- **Variable selector** (Region / Winery / etc.)  
- **Wine count range** filter for data subsetting

**📊 Visualizations:**
1. **Price Histogram**  
   → Visualizes the overall distribution of wine prices.

2. **Barplot of Wine Type vs Price**  
   → Compares prices across wine types, with quantity labels.

3. **Top Wine Regions Table**  
   → Ranks regions like Rioja and Ribera del Duero by wine count.

4. **Above vs Below Average by Winery (Stacked Bar)**  
   → Breaks down price categories by winery type (e.g., Cellars vs. Others).

> 🧠 Built with `ggplot2`, `dplyr`, and `shiny` — ideal for exploring real-world categorical and numerical data interactively.
