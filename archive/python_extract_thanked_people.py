import os
import pandas as pd
import openai
from tqdm import tqdm
import ast

# -----------------------------
# Setup paths
# -----------------------------
path_wd = "D:/Projektfolder1 (Miethe, Grosenick)/zz_AmelieMisc/"
path_data = os.path.join(path_wd, "thankyou/data/")

os.chdir(path_wd)

# -----------------------------
# Load data (RData converted to CSV for Python)
# -----------------------------
# If you have a CSV export from RData:
wos_data_ai = pd.read_csv(os.path.join(path_data, "gen/wos_data_for_ai.csv"))

# For testing, limit to first 10 rows
wos_data_ai = wos_data_ai.head(10)

# -----------------------------
# OpenAI API key
# -----------------------------
openai.api_key = "sk-..."  # replace with your key

# -----------------------------
# AI extraction function
# -----------------------------
def extract_names_ai(text: str):
    prompt = f"""
Extract only the names of people thanked for comments, suggestions, or feedback.
If someone is mentioned as editor or co-editor, write that down. For this, create a structure with two columns: 'person_name' and 'type'.
Also mention if someone is named as just edit or co-editor without name. Name is then NA.
Ignore mentions of research assistance, data access, seminar participants, or generic terms like 'all participants'.
Output the result as a Python list of dictionaries, e.g.:
[{{'person_name': 'John Doe', 'type': 'comment'}}, {{'person_name': None, 'type': 'co-editor'}}]

Text: "{text}"
"""
    try:
        response = openai.chat.completions.create(
            model="gpt-5-mini",
            messages=[{"role": "user", "content": prompt}]
        )
        result_text = response.choices[0].message.content
        
        try:
            names_list = ast.literal_eval(result_text)
        except:
            names_list = []
            
        return names_list
    except Exception as e:
        print(f"AI call failed: {e}")
        return []

import os
import pandas as pd
import openai
from tqdm import tqdm
import ast

# -----------------------------
# Paths
# -----------------------------
path_wd = "C:/Users/Amelie/Desktop/thankyou/"
path_data = path_wd
os.chdir(path_wd)

# -----------------------------
# Load data
# -----------------------------
# Convert RData to CSV first (or export from R)
wos_data_ai = pd.read_csv(os.path.join(path_data, "wos_data_for_ai.csv"))

# For testing, limit to first 10 rows
wos_data_ai = wos_data_ai.head(10)

# -----------------------------
# OpenAI API key
# -----------------------------
openai.api_key = "sk-..."  # replace with your key

# -----------------------------
# AI extraction function
# -----------------------------
def extract_names_ai(text: str):
    prompt = f"""
Extract only the names of people thanked for comments, suggestions, or feedback.
If someone is mentioned as editor or co-editor, write that down. For this, create a structure with two columns: 'person_name' and 'type'.
Also mention if someone is named as just edit or co-editor without name. Name is then NA.
Ignore mentions of research assistance, data access, seminar participants, or generic terms like 'all participants'.
Output the result as a Python list of dictionaries, e.g.:
[{{'person_name': 'John Doe', 'type': 'comment'}}, {{'person_name': None, 'type': 'co-editor'}}]

Text: "{text}"
"""
    try:
        response = openai.chat.completions.create(
            model="gpt-5-mini",
            messages=[{"role": "user", "content": prompt}]
        )
        result_text = response.choices[0].message.content
        
        try:
            names_list = ast.literal_eval(result_text)
        except:
            names_list = []
            
        return names_list
    except Exception as e:
        print(f"AI call failed: {e}")
        return []


# -----------------------------
# Loop over funding_text column
# -----------------------------
rows = []

for idx, row in tqdm(wos_data_ai.iterrows(), total=wos_data_ai.shape[0], desc="AI extracting"):
    file_name = row['file_name']
    funding_text = row['funding_text']
    
    extracted = extract_names_ai(funding_text)
    
    for entry in extracted:
        person_name = entry.get('person_name', None)
        typ = entry.get('type', None)
        rows.append({'file_name': file_name, 'person_name': person_name, 'type': typ})

# -----------------------------
# Combine into a DataFrame
# -----------------------------
df_names = pd.DataFrame(rows)

# -----------------------------
# Save results
# -----------------------------
output_file = os.path.join(path_data, "gen/ai_extracted_names_2024.csv")
df_names.to_csv(output_file, index=False)

print(df_names)
print("AI extracted names saved to:", output_file)