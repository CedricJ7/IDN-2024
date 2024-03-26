import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

d=pd.read_csv('content_10.csv',sep='\t')

n = d.shape[1]
d_new = d.iloc[:,0:4].copy()
#leonardo dicaprio
#relachement vers appuit
d_new['l->E'] = d['L->E'] - d['L->l']
d_new['e->O'] = d['E->O'] - d['E->e']
d_new['o->N'] = d['O->N'] - d['O->o']
d_new['n->A'] = d['N->A'] - d['N->n']
d_new['a->R'] = d['A->R'] - d['A->a']
d_new['r->D'] = d['R->D'] - d['R->r']
d_new['d->O'] = d['D->O'] - d['D->d']
d_new['o-> '] = d['O-> '] - d['O->o']
d_new[' ->D'] = d[' ->D'] - d[' -> ']
d_new['d->I'] = d['D->I'] - d['D->d']
d_new['i->C'] = d['I->C'] - d['I->i']
d_new['c->A'] = d['C->A'] - d['C->c']
d_new['a->P'] = d['A->P'] - d['A->a']
d_new['p->R'] = d['P->R'] - d['P->p']
d_new['r->I'] = d['R->I'] - d['R->r']
d_new['i->O'] = d['I->O'] - d['I->i']
d_new.to_csv('nouvelle_colonnes.csv',sep=';',index=False)

#relachement vers relachement
d_new['l->e'] = d['L->E'] + d['E->e']
d_new['e->o'] = d['E->O'] + d['O->o']
d_new['o->n'] = d['O->N'] + d['N->n']
d_new['n->a'] = d['N->A'] + d['A->a']
d_new['a->r'] = d['A->R'] + d['R->r']
d_new['r->d'] = d['R->D'] + d['D->d']
d_new['d->o'] = d['D->O'] + d['O->o']
d_new['o-> '] = d['O-> '] + d['O->o']
d_new[' ->d'] = d[' ->D'] + d[' -> ']
d_new['d->i'] = d['D->I'] + d['I->i']
d_new['i->c'] = d['I->C'] + d['C->c']
d_new['c->a'] = d['C->A'] + d['A->a']
d_new['a->p'] = d['A->P'] + d['P->p']
d_new['p->r'] = d['P->R'] + d['R->r']
d_new['r->i'] = d['R->I'] + d['I->i']
d_new['i->o'] = d['I->O'] + d['O->o']

#appuit vers relachement(lettre suivante)
d_new['L->e'] = d['L->E'] + d['E->e']
d_new['E->o'] = d['E->O'] + d['O->o']
d_new['O->n'] = d['O->N'] + d['N->n']
d_new['N->a'] = d['N->A'] + d['A->a']
d_new['A->r'] = d['A->R'] + d['R->r']
d_new['R->d'] = d['R->D'] + d['D->d']
d_new['D->o'] = d['D->O'] + d['O->o']
d_new['O-> '] = d['O-> '] + d['O->o']
d_new[' ->d'] = d[' ->D'] + d[' -> ']
d_new['D->i'] = d['D->I'] + d['I->i']
d_new['I->c'] = d['I->C'] + d['C->c']
d_new['C->a'] = d['C->A'] + d['A->a']
d_new['A->p'] = d['A->P'] + d['P->p']
d_new['P->r'] = d['P->R'] + d['R->r']
d_new['R->i'] = d['R->I'] + d['I->i']
d_new['I->o'] = d['I->O'] + d['O->o']

# ecrire dans un nouveau fichier csv
d_new.to_csv('content_10_new.csv',sep=';',index=False)

#moyenne entre dexux touches (relachement vers appuit)
