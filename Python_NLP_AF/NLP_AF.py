# -*- coding: utf-8 -*-
"""
Created on Fri Nov 20 08:15:22 2020

@author: David
"""

##############################################################################
## IMPORT MODEL
##############################################################################

from gensim.test.utils import common_texts
from gensim.models import Word2Vec
from pyemd import emd
from gensim.similarities import WmdSimilarity

print("start model creation")
model = Word2Vec(common_texts, size=100, window=5, min_count=1, workers=4)
word_vectors = model.wv
print("start api load")
from gensim.models import Word2Vec
import gensim.downloader as api
from gensim.test.utils import get_tmpfile
from gensim.models import KeyedVectors

word_vectors = api.load("glove-wiki-gigaword-100")

fname = get_tmpfile("vectors.kv")
word_vectors.save(fname)
word_vectors = KeyedVectors.load(fname, mmap='r')

print("end model creation and api load")

##############################################################################
## GENERATE SIMILIARITY SCORE
##############################################################################

def compare(portfolio_desc, directorate_desc):
    score =  word_vectors.n_similarity(portfolio_desc, directorate_desc)
    return score

##############################################################################
## CLEANS TEXT
##############################################################################

import re  # regex
import nltk  # natural language toolkit
# nltk.download('averaged_perceptron_tagger')
from nltk.corpus import stopwords  # for stopword removal
# nltk.download('stopwords')  # downloads list of stopwords :: only needed once
from nltk.stem import WordNetLemmatizer  # for stemming (slightly different but more accurate process)
# nltk.download('wordnet')  # downloads network of words for lemmatizing :: only needed once

# takes in text and retuns only the tokens in that text
def token_ripper(input_text):
#     print("token_ripper STARTED")
    
    input_text_stripped = input_text

##############################################################################
## STRIPS TEXT
##############################################################################

    input_text_stripped = re.sub('[^a-zA-Z]', ' ', input_text_stripped)  # replaces all non-letter characters with spaces
    input_text_stripped = re.sub('\^[a-zA-Z]\s+', ' ', input_text_stripped)  # replaces single character word at beginning of sequence with space
    while True:  # replaces remaining single character words with spaces, repeats until all single character words are cleared
        input_text_stripped = re.sub(r'\s+[a-zA-Z]\s+', ' ', input_text_stripped)
        if not (re.search(r'\s+[a-zA-Z]\s+', input_text_stripped)):
            break
    input_text_stripped = re.sub(r'\s+', ' ', input_text_stripped, flags=re.I)  # replaces all multiple spaces with single spaces
    input_text_stripped = input_text_stripped.lower()  # converts text to all lowercase
    input_text_stripped = input_text_stripped.split(' ')  # splits input text into separate words

##############################################################################
## RUNS ACRONYM REPLACER
##############################################################################
    
    input_text_stripped = acronym_replacer(input_text_stripped)
    
##############################################################################
## TRIMS TEXT
##############################################################################

    input_text_trimmed = []
    for i in range(0, len(input_text_stripped), 1):
        current_word = input_text_stripped[i]
        if len(current_word) > 3:  # CHANGED FROM 3 TO 1 THEN BACK TO 3
            input_text_trimmed.append(current_word)

##############################################################################
## REMOVE STOP WORDS
##############################################################################

    input_text_tokens = []
    all_stopwords = stopwords.words('english')  # variable to store list of stopwords
    for i in range(0, len(all_stopwords), 1):
        all_stopwords[i] = re.sub('[^a-zA-Z]', r'', all_stopwords[i])  # removes non-letter characters
        all_stopwords[i] = all_stopwords[i].lower()  # verifies all stopwords are lowercase
    for i in range(0, len(input_text_stripped), 1):
        current_word = input_text_stripped[i]
        if current_word not in all_stopwords:
            input_text_tokens.append(current_word)
    
##############################################################################
## STEMMING
##############################################################################
    
    stemmer = WordNetLemmatizer()  # lemmatizer object - essentually stems words but is more accurate than PorterStemmer
    for i in range(0, len(input_text_tokens), 1):
        current_word = input_text_tokens[i]
        current_word_stem = stemmer.lemmatize(current_word, pos='v')  # 'v' specifies verb as pos. default of noun was overwritten because mostly verbs will need to be lemmatized
        if current_word == current_word_stem:
            current_word_stem = stemmer.lemmatize(current_word, pos='a')  # 'a' specifies adjective as pos. adjective is next because of modifiers (er, est, ect.)
        if current_word == current_word_stem:
            current_word_stem = stemmer.lemmatize(current_word, pos='n')  # 'n' specifies adjective as pos. noun is last
        input_text_tokens[i] = current_word_stem
 ######## -end stemming------------------------------------------------------------
    
    processed_input_text = ' '.join(input_text_tokens)
    
#     print("token_ripper ENDED")
    return processed_input_text

##############################################################################
## ACRONYM REPLACER
##############################################################################

acronyms = {
            'AFRL': 'Air Force Research Lab', 
            'AI' : 'Artificial Intelligence',
            'USAF' : 'United States Air Force',
            'ADD' : 'Attention Deficit Disorder',
            'OCD' : 'Obsessive Compulsive Disorder',
            'PTSD' : 'Post Traumatic Stress Disorder',
            'SME' : 'Subject Matter Expert',
            'LPI' : 'Lower Probability of Intercept',
            'LPD' : 'Lower Probability of Detection',
            'ISR' : 'Intelligence Surveillance and Reconnaissance',
            'MUM-T' : 'Manned Unmanned Teaming',
            'MGUE' : 'Millitary GPS User Equipment',
            'GPS' : 'Global Positioning System',
            'ATAK' : 'Android Tactical Assault Kit',
            'SMSC' : 'System Maintenance and Support Center',
            'SMC' : 'Space and Missile Systems Center',
            'SDA' : 'Service Delivery Area',
            'USSF' : 'United States Space Force',
            'USSOCOM' : 'United States Special Operations Command',
            'PNT' : 'Positioning Navigation and Timing',
            'MIDS' : 'Multifunctional Information Distribution System',
            'TENCAP' : 'Tactical Exploitation of National Capabilities',
            'PPE' : 'Personal Protective Equipment',
            'HEPA' : 'High Efficiency Particulate Arrestance',
            'ADHD' : 'Attention Deficit Hyperactivity Disorder',
            'UAV' : 'Unmanned Aerial Vehicle',
            'USUHS' : 'Uniformed Services University of the Health Sciences',
            'DHA' : 'Department of Home Affairs',
            'AFB' : 'Air Force Base',
            'DVPRS' : 'Defense and Veterans Pain Rating Scale',
            'PDMP' : 'Prescription Drug Monitoring Program',
            'COTS' : 'Commercial Off the Shelf',
            'CAD' : 'Computer Aided Design',
            'PID' : 'Project Information Document',
            'GAO' : 'Government Accountability Office',
            'ICS' : 'Incident Command System',
            'QRNG' : 'Quantum Random Number Generator',
            'BVLOS' : 'Beyond Visual Line of Sight',
            'UAS' : 'Unmanned Aircraft System',
            'EO/IR' : 'Electro Optic Infrared',
            'CNT' : 'Carbon Nanotube Technology',
            'AI/ML' : 'Artificial Intelligence Machine Learning',
            'ML' : 'Machine Learning',
            'UI' : 'User Interface',
            'AR' : 'Augmented Reality',
            'VR' : 'Virtual Reality',
            'LEO' : 'Low Earth Orbit',
            'TBI' : 'Traumatic Brain Injury',
            'IOT' : 'Internet of Things'}

def acronym_replacer(text):
    replaced_text = text
    # split the text
#     replaced_text = replaced_text.split(' ')
    # itterate through the text
    for i in range(0, len(replaced_text), 1):
        # determine if the current word is a known acronym
        if replaced_text[i].upper() in acronyms.keys():
            # if it is, replace it with its expanded version
            replaced_text[i] = acronyms[replaced_text[i].upper()].lower()
    
    # rejoin the text
#     replaced_text = ' '.join(replaced_text)
    
    # return the text with expanded acronmys
    return replaced_text

##############################################################################
## CONNECT TO DATABASE AND EXECUTE QUERIES
##############################################################################

import mysql.connector  # for db connection
import pandas as pd  # for db data manipulation
pd.set_option('expand_frame_repr', False)
pd.set_option('display.max_colwidth', 500)
pd.set_option('display.max_columns', 10)

# frontdoor database config info
configf = {
  'user': '****',
  'password': '****',
  'host': '****',
  'database': '****',
  'raise_on_warnings': True,
  'auth_plugin':'mysql_native_password'
}

# corteza database config info
configc = {
  'user': '****',
  'password': '****',
  'host': '****',
  'database': '***',
  'raise_on_warnings': True,
  'auth_plugin':'mysql_native_password'
}

def main():
    print('main STARTED')
    
    # establish the db connection to the frontdoor db
    dbconnectionf = mysql.connector.connect(**configf)
    cursorf = dbconnectionf.cursor()
    
    # querey the directorate_keywords table
    directorate_query = "select * from directorate_keywords"
    cursorf.execute(directorate_query)
    directorate_records = cursorf.fetchall()
    directorate_df_unprocessed = pd.DataFrame(directorate_records)
#     print(directorate_df_unprocessed)
    # process directorate_df_unprocessed into a usable format
    directorate_list = []
    directorate_keywords_list = []
    # get a list of all directorates
    for i in range(0, len(directorate_df_unprocessed), 1):
        current_directorate = directorate_df_unprocessed.loc[i, 0]
        if not current_directorate in directorate_list:
            directorate_list.append(current_directorate)
#     print(directorate_list)
    # get a list of current directorate keywords and organize a list of tuples to be put into a dataframe
    directorate_tuples_list = []
    for i in range(0, len(directorate_list), 1):
        current_directorate_keywords = []
        for j in range(0, len(directorate_df_unprocessed), 1):
            if directorate_list[i] == directorate_df_unprocessed.loc[j, 0]:
                current_directorate_keywords.append(directorate_df_unprocessed.loc[j, 1])
        directorate_tuples_list.append((directorate_list[i], ", ".join(current_directorate_keywords)))
#     print(directorate_tuples_list)
    # put the tuple data into a pandas dataframe
    directorate_df = pd.DataFrame(directorate_tuples_list, columns=['directorate','keywords'])
    print(directorate_df)

    # establish the db connection to the corteza db
    dbconnectionc = mysql.connector.connect(**configc)
    cursorc = dbconnectionc.cursor()
    
    real_ideas_querey = "select idea_number, field_name, field_value from `ideas` where field_name = 'title' or field_name = 'idea' or field_name = 'assigned_directorate'"
    cursorc.execute(real_ideas_querey)
    real_ideas_records = cursorc.fetchall()
    real_ideas_df_unprocessed = pd.DataFrame(real_ideas_records)
#     print(real_ideas_df_unprocessed)
    # process real_ideas_df_unprocessed into usable format
    real_ideas_tuple_list = []  # tuple format: (title, idea, assigned_directorate)
    title = "NONE"
    idea = "NONE"
    assigned_directorate = ""
    i = 1
    while i < len(real_ideas_df_unprocessed):
#     for i in range(0, len(real_ideas_df_unprocessed), 1):
#         print(real_ideas_df_unprocessed.loc[i])
        number = real_ideas_df_unprocessed.loc[i-1, 0]
        j = i-1
        curr_number = number
        while curr_number == number and j < len(real_ideas_df_unprocessed) - 1:
            if real_ideas_df_unprocessed.loc[j, 1] == 'assigned_directorate':
                assigned_directorate = assigned_directorate + real_ideas_df_unprocessed.loc[j, 2] + " "
            if real_ideas_df_unprocessed.loc[j, 1] == 'idea':
                idea = real_ideas_df_unprocessed.loc[j, 2]
            if real_ideas_df_unprocessed.loc[j, 1] == 'title':
                title = real_ideas_df_unprocessed.loc[j, 2]
            j = j + 1
            curr_number = real_ideas_df_unprocessed.loc[j, 0]
        if assigned_directorate == "":
            assigned_directorate = "NONE"
        real_ideas_tuple_list.append((number, title, idea, assigned_directorate))
        title = "NONE"
        idea = "NONE"
        assigned_directorate = ""
        i = j
        i = i + 1
#     print(real_ideas_tuple_list)
    # create a dataframe from this list of tuples
    real_ideas_df = pd.DataFrame(real_ideas_tuple_list, columns=['idea_number', 'title', 'idea', 'assigned_directorate'])
#     print(real_ideas_df)
        
    # generte a list of tokenized directorate descriptions
    directorate_descs_tokenized = []
    for i in range(0, len(directorate_df), 1):
        directorate_descs_tokenized.append(token_ripper(directorate_df.iloc[i]['keywords']))
#     print(directorate_descs_tokenized)
    
    # generate a list of tokenized real idea descriptions
    real_ideas_tokenized = []
    for i in range(0, len(real_ideas_df), 1):
        real_ideas_tokenized.append(token_ripper(real_ideas_df.iloc[i]['idea']))
#     print(real_ideas_tokenized)

    # strip tokenized directorate descriptions of any words not in the model
    for i in range(0, len(directorate_descs_tokenized), 1):
        directorate_descs_tokenized[i] = directorate_descs_tokenized[i].split(' ')
        temp = []
        for j in range(0, len(directorate_descs_tokenized[i]), 1):
            if directorate_descs_tokenized[i][j] in word_vectors:
                temp.append(directorate_descs_tokenized[i][j])
#             else:
#                 print(directorate_descs_tokenized[i][j])
        directorate_descs_tokenized[i] = temp
    
    # strip tokenized real idea descriptions of any words not in the model
    for i in range(0, len(real_ideas_tokenized), 1):
        real_ideas_tokenized[i] = real_ideas_tokenized[i].split(' ')
        temp = []
        for j in range(0, len(real_ideas_tokenized[i]), 1):
            if real_ideas_tokenized[i][j] in word_vectors:
                temp.append(real_ideas_tokenized[i][j])
#             else:
#                 print(real_ideas_tokenized[i][j])
        real_ideas_tokenized[i] = temp

    # compare each entry in real ideas to directorates
    for i in range(0, len(real_ideas_tokenized), 1):
        comparison_df = pd.DataFrame(columns=[
            'directorate_name',
            'directorate_keywords',
            'idea_number',
            'idea_title',
            'idea_description',
            'match_score'])
        idea = real_ideas_tokenized[i]
        print(real_ideas_df.iloc[i]['title'])
        print(real_ideas_df.iloc[i]['idea_number'])
#         print(real_ideas_df.iloc[i]['idea'])
#         print(idea)
        print(real_ideas_df.iloc[i]['assigned_directorate'])
        if idea:
            for j in range(0, len(directorate_descs_tokenized), 1):
                directorate_desc = directorate_descs_tokenized[j]
                if directorate_desc:
                    comparison_df = comparison_df.append({
                        'directorate_name': directorate_df.iloc[j]['directorate'],
                        'directorate_keywords' : directorate_desc,
                        'idea_number': real_ideas_df.iloc[i]['idea_number'],
                        'idea_title': real_ideas_df.iloc[i]['title'],
                        'idea_description': real_ideas_df.iloc[i]['idea'],
                        'match_score': compare(idea, directorate_desc)
                    }, ignore_index=True)
        if not comparison_df.empty:
#             print(comparison_df.nlargest(3, ['match_score'])[['match_score', 'directorate_name', 'directorate_keywords']])
            print(comparison_df.nlargest(3, ['match_score'])[['match_score', 'directorate_name']])
        else:
            print("ERROR: COMPARISON DATAFRAME EMPTY. Idea or Directorate description contained no keywords that were in word_vectors. Investigate.")
        
    
    print("main ENDED")
    return 0

if __name__ == '__main__':
