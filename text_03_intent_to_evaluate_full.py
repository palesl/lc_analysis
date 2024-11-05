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
    df = df.loc[0:199,]

r, c = df.shape

if debug:
    print(str(c) + " columns")
    print(str(r) + " rows")

### (1c) Check outfile doesn't already exist
outfile = file.replace("text_03_intent_flan_full.rds", "text_03_flan_full.csv")

##if os.path.isfile(outfile):
##    print("Skipping " + file)
##    exit()

### (3) Set up the classification stuff
class_req = "does the following passage include an opinion about the "

class_post = "? Classify as Yes or No: \ "

input_text = [ class_req + df["law"][idx] + class_post + df["text"][idx] + "\"" for idx in range(r)]

classed_out = [out["generated_text"] for out in pipe(input_text)]

df = df.assign(flan_class=classed_out)

df = df.loc[:,['austlii_id', 'href','text', 'flan_class']]
 
df.to_csv(outfile, index = False)
