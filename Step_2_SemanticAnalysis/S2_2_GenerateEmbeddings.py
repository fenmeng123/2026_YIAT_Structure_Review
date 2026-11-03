# Author: Kunru Song
"""Generate formal vectors from supplied texts and fixed offline model snapshots.

Existing formal RDS files are reused by default. Pass --overwrite only when a
deliberate re-encoding is intended. Statistical analyses are implemented in R.
"""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import subprocess
import tempfile

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--root', type=Path, default=Path.cwd())
parser.add_argument('--model', default='all', choices=['all', 'mpnet', 'personality', 'harmony', 'multilingual_mpnet'])
parser.add_argument('--overwrite', action='store_true')
args = parser.parse_args()
root = args.root.resolve()
inputs_dir = root / 'Res_1_Data_for_analyses'
inputs = json.loads((inputs_dir / 'Semantic_Inputs.json').read_text(encoding='utf-8-sig'))
config = json.loads((inputs_dir / 'YIAT_Encoder_Configuration.json').read_text(encoding='utf-8-sig'))
os.environ.update(HF_HUB_OFFLINE='1', TRANSFORMERS_OFFLINE='1', HF_DATASETS_OFFLINE='1',
                  HF_HUB_DISABLE_XET='1', TOKENIZERS_PARALLELISM='false',
                  HF_HOME=str(inputs_dir / 'Frozen_Encoders'))

import numpy as np
import torch
from sentence_transformers import SentenceTransformer

torch.set_num_threads(4)
torch.use_deterministic_algorithms(True)
for specification in config['models']:
    name = specification['name']
    if args.model not in ('all', name):
        continue
    pending = []
    for run in specification['runs']:
        target = inputs_dir / f"embeddings_formal_{name}_{run['version']}_definition.rds"
        if target.exists() and not args.overwrite:
            print(f'Reuse {target.name}', flush=True)
        else:
            pending.append((run, target))
    if not pending:
        continue
    snapshot = (inputs_dir / 'Frozen_Encoders' /
                ('models--' + specification['model_id'].replace('/', '--')) /
                'snapshots' / specification['revision'])
    encoder = SentenceTransformer(str(snapshot), device='cpu', local_files_only=True, trust_remote_code=False)
    encoder.eval()
    encoder.requires_grad_(False)
    for run, target in pending:
        seed = run['seed']
        torch.manual_seed(seed)
        np.random.seed(seed)
        version = run['version']
        items = inputs['versions'][version]
        domains = inputs['domains']
        texts = [row['item_text'] for row in items] + [row['definition_text'] for row in domains]
        # A model's sequence limit must not silently truncate scientific input text.
        for text in texts:
            tokens = encoder.tokenizer.encode(text, add_special_tokens=True, truncation=False)
            if len(tokens) > encoder.max_seq_length:
                raise ValueError(f'{name}/{version}: input exceeds the fixed sequence limit.')
        with torch.inference_mode():
            vectors = encoder.encode(texts, batch_size=16, show_progress_bar=False,
                                     normalize_embeddings=True, convert_to_numpy=True)
        item_rows = [dict(row, encoded_text=row['item_text'], vector=vector.tolist())
                     for row, vector in zip(items, vectors[:len(items)])]
        domain_rows = [dict(row, encoded_text=row['definition_text'], vector=vector.tolist())
                       for row, vector in zip(domains, vectors[len(items):])]
        result = dict(author='Kunru Song', model_name=name, model_id=specification['model_id'],
                      revision=specification['revision'], item_version=version, variant_id='definition',
                      run_type='formal', dimension=int(vectors.shape[1]), seed=seed,
                      normalization='unit_l2', items=item_rows, domains=domain_rows)
        # R owns the RDS serialization; JSON is a temporary data-transfer format.
        handle, temporary_name = tempfile.mkstemp(prefix='embedding_transfer_', suffix='.json', dir=inputs_dir)
        os.close(handle)
        temporary = Path(temporary_name)
        try:
            temporary.write_text(json.dumps(result, ensure_ascii=False, allow_nan=False), encoding='utf-8')
            r_code = ("paths <- commandArgs(trailingOnly=TRUE); "
                      "embedding <- jsonlite::read_json(paths[1], simplifyVector=TRUE, simplifyMatrix=FALSE); "
                      "saveRDS(embedding, paths[2])")
            subprocess.run([r'D:\Program Files\R\R-4.5.3\bin\Rscript.exe', '-e', r_code,
                            str(temporary), str(target)], check=True)
        finally:
            temporary.unlink(missing_ok=True)
        print(f'Wrote {target.name}', flush=True)
