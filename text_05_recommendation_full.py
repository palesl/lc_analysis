### Purpose of this code: take in a named .rds file with debates,
### convert to pandas, classify sentences in it, save the results
import os, sys
import pyreadr ## to get the RDS files in

from transformers import pipeline



#multiprocessing_context='fork' if torch.backends.mps.is_available() else None

debug = False

pipe = pipeline("text2text-generation",
                model="google/flan-t5-large",
                device='mps',
                batch_size = 30,
                num_workers = 0)
                
                # i think for apple silicon, the best option is to use batches.

### (1) Get the file in
#file = sys.argv[1]


file = "/Users/pat/Library/CloudStorage/Dropbox/2021_ARC_Pathways to Power/Legislative Quality/Methodology/data/text/text_03_intent_flan_full.rds"

try:
    ## import the file (https://github.com/ofajardo/pyreadr)
    result = pyreadr.read_r(file)
    df = result[None]
except OSError as err:
    print("OS error:", err)
except Exception as err:
    print(f"Unexpected {err=}, {type(err)=}")
    raise


### (1b) Subset
if debug:
    low=41000
    newrange=range(low,low+200)
    df = df.loc[newrange,]

r, c = df.shape

 
if debug:
    print(str(c) + " columns")
    print(str(r) + " rows")

 
### (1c) Check outfile doesn't already exist
outfile = file.replace("text_05_intent_flan_full.rds", "text_05_flan_recommend_full.csv")

##if os.path.isfile(outfile):
##    print("Skipping " + file)
##    exit()

### (3) Set up the classification stuff
class_req = "Does the following passage recommend a change to the "

class_post = "? Classify as Yes or No: "

input_text = [ class_req + df["law"][idx] + class_post +"\""+ df["text"][idx] + "\"" for idx in range(r)]

classed_out = [out["generated_text"] for out in pipe(input_text)]

df = df.assign(flan_class_rec=classed_out)

df = df.loc[:,['austlii_id', 'href','text', 'flan_class_rec']]
 
df.to_csv(outfile, index = False)
