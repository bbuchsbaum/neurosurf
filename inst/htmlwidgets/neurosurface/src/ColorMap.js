import colormap from 'colormap';
import { EventEmitter } from './EventEmitter.js';
import { debugLog } from './debug.js';

class ColorMap extends EventEmitter {
  constructor(colors, options = {}) {
    super();
    if (!Array.isArray(colors) || colors.length === 0) {
      throw new TypeError('Colors must be a non-empty array');
    }

    this.colors = colors.map(color => this.parseColor(color));
    this._hasAlpha = this.colors[0].length === 4;
    this.setRange(options.range);
    this.setThreshold(options.threshold);
    this.setAlpha(options.alpha);
  }

  setRange(range) {
    if (Array.isArray(range) && range.length === 2 && range.every(v => typeof v === 'number')) {
      this.range = range;
      debugLog('ColorMap: Emitting rangeChanged event', this.range);
      this.emit('rangeChanged', this.range);
    } else {
      this.range = [0, 1];
    }
  }

  setThreshold(threshold) {
    if (Array.isArray(threshold) && threshold.length === 2 && threshold.every(v => typeof v === 'number')) {
      this.threshold = threshold;
      debugLog('ColorMap: Emitting thresholdChanged event', this.threshold);
      this.emit('thresholdChanged', this.threshold);
    } else {
      this.threshold = [0, 0];
    }
  }

  /**
   * Normalize and validate a color specification.
   * Colors may be provided as a hex string (with or without leading '#') or as
   * an array of numeric components in the range [0, 1].
   *
   * @param {string|number[]} color The color to parse.
   * @returns {number[]} Array of normalized RGB or RGBA components.
   */
  parseColor(color) {
    if (typeof color === 'string' && color.startsWith('#')) {
      return this.hexToRgb(color);
    }
    if (
      Array.isArray(color) &&
      (color.length === 3 || color.length === 4) &&
      color.every(v => Number.isFinite(v) && v >= 0 && v <= 1)
    ) {
      return color;
    }
    throw new TypeError('Invalid color format');
  }

  /**
   * Convert a hex color string to normalized RGB(A) components.
   *
   * @param {string} hex Hexadecimal color string starting with '#'. May include
   *                     an optional alpha channel.
   * @returns {number[]} Array of RGB or RGBA components in the range [0, 1].
   */
  hexToRgb(hex) {
    hex = hex.replace(/^#/, '');

    if (!/^[0-9a-fA-F]{6}([0-9a-fA-F]{2})?$/.test(hex)) {
      throw new Error('Invalid hex color');
    }

    const r = parseInt(hex.slice(0, 2), 16) / 255;
    const g = parseInt(hex.slice(2, 4), 16) / 255;
    const b = parseInt(hex.slice(4, 6), 16) / 255;

    if (hex.length === 8) {
      const a = parseInt(hex.slice(6, 8), 16) / 255;
      return [r, g, b, a];
    }

    return [r, g, b];
  }

  setAlpha(alpha) {
    if (alpha === undefined) {
      this.colors = this.colors.map(color => color.length === 3 ? [...color, 1] : color);
    } else if (typeof alpha === 'number' && alpha >= 0 && alpha <= 1) {
      this.colors = this.colors.map(color => [...color.slice(0, 3), alpha]);
    } else if (Array.isArray(alpha) && alpha.length === this.colors.length && alpha.every(v => typeof v === 'number' && v >= 0 && v <= 1)) {
      this.colors = this.colors.map((color, i) => [...color.slice(0, 3), alpha[i]]);
    } else {
      throw new TypeError('Invalid alpha parameter');
    }
    this._hasAlpha = true;
    this.emit('alphaChanged', this._hasAlpha);
  }

  getColor(value) {
    if (typeof value !== 'number' || !Number.isFinite(value)) {
      throw new TypeError('Value must be a finite number');
    }
    const [min, max] = this.range;
    const [low, high] = this.threshold;
    
    // Apply threshold
    if (value >= low && value <= high) {
      return [0, 0, 0, 0]; // Fully transparent
    }
    
    if (max === min) {
      const color = this.colors[0];
      return color.length === 3 ? [...color, 1] : color;
    }

    const normalizedValue = (value - min) / (max - min);
    const index = Math.min(Math.max(Math.floor(normalizedValue * (this.colors.length - 1)), 0), this.colors.length - 1);
    const color = this.colors[index];
    return color.length === 3 ? [...color, 1] : color; // Ensure returned color has alpha channel
  }

  getColorArray(values) {
    if (!(Array.isArray(values) || values instanceof Float32Array) || values.length === 0) {
      throw new TypeError('Values must be a non-empty array or Float32Array');
    }
    
    const componentsPerColor = this.hasAlpha ? 4 : 3;
    const colorArray = new Float32Array(values.length * componentsPerColor);
    
    for (let i = 0; i < values.length; i++) {
      const value = values[i];
      if (!Number.isFinite(value)) {
        throw new TypeError(`Value at index ${i} must be a finite number`);
      }
      const color = this.getColor(value);
      const offset = i * componentsPerColor;
      colorArray[offset] = color[0];
      colorArray[offset + 1] = color[1];
      colorArray[offset + 2] = color[2];
      if (this.hasAlpha) {
        colorArray[offset + 3] = color[3];
      }
    }

    return colorArray;
  }

  getColors(values) {
    if (!Array.isArray(values) || !values.every(v => typeof v === 'number')) {
      throw new TypeError('Values must be an array of numbers');
    }
    return values.map(this.getColor.bind(this));
  }

  get hasAlpha() {
    return this._hasAlpha;
  }

  static presetMaps = {
    jet: ColorMap.generatePreset('jet'),
    density: ColorMap.generatePreset('density'),
    freesurfaceBlue: ColorMap.generatePreset('freesurface-blue'),
    freesurfaceRed: ColorMap.generatePreset('freesurface-red'),
    oxygen: ColorMap.generatePreset('oxygen'),
    par: ColorMap.generatePreset('par'),
    phase: ColorMap.generatePreset('phase'),
    salinity: ColorMap.generatePreset('salinity'),
    temperature: ColorMap.generatePreset('temperature'),
    turbidity: ColorMap.generatePreset('turbidity'),
    velocityBlue: ColorMap.generatePreset('velocity-blue'),
    velocityGreen: ColorMap.generatePreset('velocity-green'),
    cubehelix: ColorMap.generatePreset('cubehelix')
  };

  static generatePreset(name) {
    return colormap({
      colormap: name,
      nshades: 256,
      format: 'hex',
      alpha: 1
    });
  }

  static getAvailableMaps() {
    return Object.keys(ColorMap.presetMaps);
  }

  static fromPreset(name, options = {}) {
    if (!ColorMap.presetMaps.hasOwnProperty(name)) {
      throw new Error(`Preset "${name}" not found`);
    }
    const presetColors = ColorMap.presetMaps[name];
    
    if (options.existingColorMap instanceof ColorMap) {
      return options.existingColorMap.copy(presetColors);
    }
    
    return new ColorMap(presetColors, options);
  }

  copy(newColors) {
    const options = {
      range: this.range,
      threshold: this.threshold,
      alpha: this._hasAlpha ? this.colors.map(color => color[3]) : undefined
    };

    return new ColorMap(newColors, options);
  }
}

export default ColorMap;
