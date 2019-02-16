#Tratamento dos dados 

#Remoção dos dados onde não tinha o valor do parametro idade(AgeNorm)
dataset = dataset.dropna(subset=['AgeNorm'])

#Remoção dos dados onde não tinha o valor do parametro Sexo(Gender)
dataset = dataset.drop(dataset[dataset['Gender'].astype(str) == 'Not Available'].index)
dataset = dataset.drop(dataset[dataset['Gender'].astype(str) == 'Not Reported'].index)
dataset = dataset.drop(dataset[dataset['Gender'].astype(str) == 'Unknown'].index)

#Remoção dos dados onde não tinha o valor do parametro Nome da Industria(IndustryName)
dataset = dataset.drop(dataset[dataset['IndustryName'].astype(str) == 'NaN'].index)

#Remoção dos dados onde não tinha o valor do parametro Sintomas(Symptoms)
dataset = dataset.drop(dataset[dataset['Symptoms'].astype(str) == 'NaN'].index)

#Remove as colunas/parametros de idade não normalizada, unidade da idade.
dataset_cl = dataset.drop(['Age', 'AgeUnit'], axis=1)     

dataset = dataset_cl

#Balanceando os dados
sample_size = 20000
np.random.seed(42)
review_1 = review[review['stars']==1].sample(sample_size,replace=True)
review_2 = review[review['stars']==2].sample(sample_size,replace=True)
review_3 = review[review['stars']==3].sample(sample_size,replace=True)
review_4 = review[review['stars']==4].sample(sample_size,replace=True)
review_5 = review[review['stars']==5].sample(sample_size,replace=True)
review = pd.concat([review_1,review_2,review_3,review_4,review_5])
review['stars'].value_counts()

# check the distribution of the data
review = merge[['text','stars']]
review['stars'].value_counts()

# Definição e execução do pipeline
from sklearn.preprocessing import StandardScaler
pipe_lr = Pipeline([('scl', StandardScaler()),
                    ('pca', PCA(n_components=2)),
                    ('clf', LogisticRegression(random_state=1))])
pipe_lr.fit(X_train, y_train)
print('Acurácia do teste: %.3f' % pipe_lr.score(X_test, y_test))

#refitting on entire training data using best settings
clf.refit

preds = clf.predict(X_test)
probs = clf.predict_proba(X_test)

np.mean(preds == y_test)