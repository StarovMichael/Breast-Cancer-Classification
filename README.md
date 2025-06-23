1. Project Overview. 
The aim of this project was to develop and implement a Support Vector Machine (SVM) algorithm from scratch (without built-in libraries) to classify breast cancer tumors as either malignant or benign. The model was trained and evaluated on a publicly available medical dataset from Kaggle, achieving high interpretability and accuracy.

2. The dataset consists of 569 medical records, each representing patient data about breast tumors with 30 numeric attributes describing the geometry, texture, and structural characteristics of cells:
- Mean values: radius_mean, texture_mean, perimeter_mean, etc.
- Standard errors (se): radius_se, texture_se, perimeter_se, etc.
- Worst values: radius_worst, texture_worst, perimeter_worst, etc.

Target Variable: diagnosis
- M = Malignant tumor (coded as +1)
- B = Benign tumor (coded as -1)

3. Methodology & Implementation
- Algorithm
    - Support Vector Machine (SVM) algorithm manually implemented using quadratic programming (quadprog in R).
    - The algorithm finds an optimal separating hyperplane maximizing the margin between two classes, ensuring robustness and preventing overfitting.
- Data Preprocessing
    - Removed irrelevant and correlated attributes (correlation threshold >0.9).
    - Encoded diagnosis attribute into binary format (+1 for malignant, -1 for benign).
    - Split dataset into training (70%) and testing (30%) subsets.
    - Applied standardization based solely on training data metrics (mean and standard deviation).
- Evaluation Metrics. Manually implemented key classification metrics:
    - Accuracy: 93.6%
    - Precision: 90.8%
    - Recall: 92.2%
    - F1-score: 91.5%
    - ROC Curve: Area Under Curve (AUC) = 0.9851

4.  Interactive Web Application (R Shiny).
An interactive R Shiny web app was developed to allow medical professionals to easily classify tumors by entering measured attributes. Key features include:
- User-friendly interface clearly indicating acceptable ranges for each input.
- Real-time validation of inputs.
- Immediate visualization of classification results:
- Malignant: clearly displayed in red.
0 Benign: clearly displayed in green.
