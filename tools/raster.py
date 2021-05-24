import xml.etree.ElementTree as ET
import subprocess

namespace = {
    'svg': 'http://www.w3.org/2000/svg'
}


def export_pngs(svg_file, tmp_dir, export_dir, scale):
    tree = ET.parse(svg_file)
    root = tree.getroot()

    layers = root.findall('.//svg:g', namespace)

    for layer in layers:
        label = layer.attrib['{http://www.inkscape.org/namespaces/inkscape}label']
        layer.set('style', 'display:inline')
        for other in layers:
            if other != layer:
                other.set('style', 'display:none')
        tree.write('tmp.svg')
        subprocess.run([
            'inkscape',
            '--export-type=png',
            f'--export-width={scale[0]}',
            f'--export-height={scale[1]}',
            f'{tmp_dir}/tmp.svg'])
        subprocess.run([
            'mv',
            f'{tmp_dir}/tmp.png',
            f'{export_dir}/{label}.png'])

    subprocess.run([
        'rm',
        f'tmp.svg'])


def main():
    svg_dir = '../svgs'
    asset_dir = '../assets'
    svgs = [
        ('tile', (20, 20)),
        ('digit', (19, 33)),
        ('bg_large', (630, 416)),
        ('bg_medium', (350, 416)),
        ('bg_small', (210, 276)),
        ('face', (42, 42)),
        ('digit_panel', (65, 37))]
    for name, size in svgs:
        export_pngs(f'{svg_dir}/{name}.svg', '.', asset_dir, size)


if __name__ == '__main__':
    main()
