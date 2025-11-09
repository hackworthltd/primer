import path from 'path';
import { fileURLToPath } from 'url';
import { rspack } from '@rspack/core';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export default (_env = {}, argv = {}) => {
  const mode = argv.mode ?? process.env.NODE_ENV ?? 'production';

  return {
    mode,
    entry: './frontend/index.js',
    output: {
      path: path.resolve(__dirname, 'dist'),
    },
    devtool: mode === 'development' ? 'source-map' : false,
    experiments: {
      asyncWebAssembly: true,
    },
    builtins: {
      css: {
        modules: 'global',
      },
    },
    module: {
      rules: [
        {
          test: /\.css$/i,
          use: [
            rspack.CssExtractRspackPlugin.loader,
            {
              loader: 'css-loader',
              options: {
                sourceMap: mode === 'development',
              },
            },
          ],
        },
        {
          test: /\.wasm$/i,
          type: 'asset/resource',
        },
      ],
    },
    plugins: [
      new rspack.CssExtractRspackPlugin(),
    ],
  };
};
