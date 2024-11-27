import resolve from '@rollup/plugin-node-resolve';

import pkg from './package.json';

export default {
  input: 'src/index.js',
  external: Object.keys(pkg.dependencies),
  plugins: [
    resolve({
      modulePaths: ['src/node_modules', 'node_modules'], // Specify your custom directory first
    }),
  ],
  output: [
    { format: 'cjs', file: pkg.exports.require, exports: 'auto' },
    { format: 'esm', file: pkg.exports.import }
  ]
};
