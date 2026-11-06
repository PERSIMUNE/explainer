"""Export the freshly rendered tutorial's SHAP data without altering its results."""
import json
import re
from pathlib import Path

article = Path('docs/articles/explainer_tutorial_binary_classification.html').read_text()
widgets = re.findall(r'<script type="application/json" data-for="[^"]+">(.*?)</script>', article)
if not widgets:
    raise RuntimeError('The rebuilt tutorial contains no interactive SHAP output')
widget = json.loads(widgets[0])['x']
features = widget['layout']['yaxis']['ticktext']
expected = {'Cl.thickness', 'Cell.size', 'Cell.shape', 'Marg.adhesion', 'Epith.c.size',
            'Bare.nuclei', 'Bl.cromatin', 'Normal.nucleoli', 'Mitoses'}
if set(features) != expected:
    raise RuntimeError(f'Unexpected SHAP predictors: {features!r}')
plot = {'data': widget['data'], 'layout': widget['layout'],
        'config': {'responsive': True, 'displaylogo': False, 'scrollZoom': False,
                   'modeBarButtonsToRemove': ['sendDataToCloud']}}
plot['layout'].update(autosize=True, height=600,
                      margin={'l': 145, 'r': 150, 't': 35, 'b': 80}, paper_bgcolor='white')
plot['layout']['legend']['font']['size'] = 11
page = Path('community/huggingface/shap-plot.html')
text = page.read_text()
pattern = r'(<script type="application/json" id="plot-data">).*?(</script>)'
if len(re.findall(pattern, text, re.S)) != 1:
    raise RuntimeError('Expected exactly one showcase plot data block')
data = json.dumps(plot, separators=(',', ':')).replace('</', '<\\/')
text = re.sub(pattern, lambda m: m[1] + data + m[2], text, flags=re.S)
page.write_text(text)
print('Exported SHAP results for the nine recorded cell-characteristic predictors.')
