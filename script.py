import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
from sklearn.metrics import classification_report, confusion_matrix
# %matplotlib inline

data = pd.read_csv("./bill_authentication.csv")
# data = pd.read_csv("./Datasets/Leukemia/Leukemia.txt", sep='\t')

# zapisane ze srednikami jako separator i nazwa komorki zmieniona na Class
data = pd.read_csv("./Leukemia_class.csv").transpose()

# print(data)
# print(data.shape)
# print(data.head())

X = data.drop(0, axis=1)
y = data[0][1:]
print(y)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.20)
svclassifier = SVC(kernel='linear')
svclassifier.fit(X_train, y_train)

y_pred = svclassifier.predict(X_test)

print(confusion_matrix(y_test,y_pred))
print(classification_report(y_test,y_pred))