import path from 'path';
import { fileURLToPath } from 'url';
import { rspack } from '@rspack/core';
import HtmlRspackPlugin from 'html-rspack-plugin';

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
        {
          test: /\.(woff2?|otf|ttf)$/i,
          type: 'asset/resource',
          generator: {
            filename: 'fonts/[name][ext]',
          },
        },
      ],
    },
    plugins: [
      new rspack.CssExtractRspackPlugin(),
      new HtmlRspackPlugin({
        template: './frontend/index.html',
      }),
      new rspack.CopyRspackPlugin({
        patterns: [
          {
            from: path.resolve(__dirname, 'frontend/generated/bin.wasm'),
            to: 'bin.wasm',
          },
          {
            from: path.resolve(__dirname, 'frontend/generated/bin.wasm.br'),
            to: 'bin.wasm.br',
            noErrorOnMissing: true,
          },
        ],
      }),
    ],
  };
};
