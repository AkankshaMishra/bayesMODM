# ✅ Import libraries
import pandas as pd
from huggingface_hub import login
import os
from transformers import AutoTokenizer, AutoModelForCausalLM, pipeline
import pandas as pd
from itertools import combinations
from tqdm import tqdm
import re
import random

# Paste your token here (in quotes)
login("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")

# ✅ Load the Phi-2 model from Hugging Face (free and open)
model_id = "microsoft/phi-2"
tokenizer = AutoTokenizer.from_pretrained(model_id)
model = AutoModelForCausalLM.from_pretrained(model_id, device_map="auto", trust_remote_code=True)
llm = pipeline("text-generation", model=model, tokenizer=tokenizer)

# ✅ Load your candidate portfolios
portfolios = pd.read_csv("populations.csv").values
n = portfolios.shape[0]

# ✅ Generate unique portfolio pairs
pairs = list(combinations(range(n), 2))
print("Number of pairs:", len(pairs))

company_names = ['AAPL', 'AMZN', 'BABA', 'BBY', 'GE', 'GM', 'GOOG', 'MA', 'PFE', 'RRC', 'SBUX', 'T', 'WMT', 'XOM']
def format_portfolio(portfolio):
    return ', '.join(f"{name}: {weight:.6f}" for name, weight in zip(company_names, portfolio))

# ✅ Preference function using Hugging Face model
def get_preference(portfolio1, portfolio2):
    prompt = (
    "You are an investment analyst. Compare the following two portfolios and decide which one is more attractive for investment.\n\n"
    f"Portfolio A:\n[{format_portfolio(portfolio1)}]\n\n"
    f"Portfolio B:\n[{format_portfolio(portfolio2)}]\n\n"
    "Each entry represents the investment weight assigned to a company.\n"
    "Assume higher weight indicates higher investment preference in that stock.\n"
    "Which portfolio is more attractive overall, considering potential returns and diversification and 20 years historical data?\n\n"
    "Answer with only one letter: 'A' or 'B'.\n"
    "Answer:"
)
    inputs = tokenizer(prompt, return_tensors="pt").to(model.device)
    outputs = model.generate(
        **inputs,
        max_new_tokens=10,
        do_sample=True,
        temperature=0.7,
        pad_token_id=tokenizer.eos_token_id,
    )

    response = tokenizer.decode(outputs[0], skip_special_tokens=True)
    # print("🔹 Prompt:\n", prompt)
    print("🔹 Generated:\n", response)

    # Logic to parse answer
    # Extract only the part after the full prompt
    answer_block = response[len(prompt):].strip()

    # Use regex to find the first standalone 'A' or 'B'
    match = re.search(r'\b([AB])\b', answer_block)
    if match:
        answer = match.group(1)
        print("✅ Parsed answer:", answer)
        return 1 if answer == "A" else 0

    print("⚠️ Could not parse a valid answer.")
    return None

# ✅ Run preferences for all pairs
results = []
for i, j in tqdm(pairs):  # Limit for demo
    pref = get_preference(portfolios[i], portfolios[j])
    print("Pref", pref)
    results.append({"portfolio_i": i, "portfolio_j": j, "preference": pref})

# ✅ Save and show results
pref_df = pd.DataFrame(results)
pref_df.to_csv("temp.csv", index=False)
print("✅ Preference results saved to 'temp.csv'")