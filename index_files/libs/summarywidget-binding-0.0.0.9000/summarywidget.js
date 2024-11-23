HTMLWidgets.widget({
  name: 'summarywidget',
  type: 'output',

  factory: function(el, width, height) {

    // Function to filter an object based on keys
    var filterKeys = function(obj, keys) {
      var result = {};
      keys.forEach(function(k) {
        if (obj.hasOwnProperty(k))
          result[k] = obj[k];
      });
      return result;
    };

    return {
      renderValue: function(x) {

        // Prepare data object with keys
        var data = {};
        var i;
        for (i = 0; i < x.data.length; i++) {
          var key = x.key ? x.key[i] : i;
          data[key] = {
            value: x.data[i],
            numerator_flag: x.numerator_flag ? x.numerator_flag[i] === true : false,
            denominator_flag: x.denominator_flag ? x.denominator_flag[i] === true : false,
            numerator_value: x.numerator_value ? x.numerator_value[i] : null,
            denominator_value: x.denominator_value ? x.denominator_value[i] : null
          };
        }

        // Function to update the display based on filtered data
        var update = function(d) {
          // Convert object to array
          var dataArray = [];
          for (var key in d) {
            if (d.hasOwnProperty(key)) {
              dataArray.push(d[key]);
            }
          }

          var value = 0; // Initialize the value to display

          // Compute the statistic based on the selected type
          switch (x.settings.statistic) {
            case 'count_rate':
              // Filter numerator and denominator data
              var numeratorData = dataArray.filter(function(d) { return d.numerator_flag; });
              var denominatorData = dataArray.filter(function(d) { return d.denominator_flag; });
              var numeratorCount = numeratorData.length;
              var denominatorCount = denominatorData.length;
              value = denominatorCount === 0 ? 0 : (numeratorCount / denominatorCount * 100);
              break;

            case 'numeric_rate':
              // Filter numerator and denominator data
              var numeratorData = dataArray.filter(function(d) { return d.numerator_flag; });
              var denominatorData = dataArray.filter(function(d) { return d.denominator_flag; });
              // Sum numerator and denominator values
              var numeratorSum = numeratorData.reduce(function(acc, d) {
                return acc + (d.numerator_value || 0);
              }, 0);
              var denominatorSum = denominatorData.reduce(function(acc, d) {
                return acc + (d.denominator_value || 0);
              }, 0);
              value = denominatorSum === 0 ? 0 : (numeratorSum / denominatorSum * 100);
              break;

            case 'count':
              value = dataArray.length;
              break;

            case 'sum':
              value = dataArray.reduce(function(acc, d) { return acc + d.value; }, 0);
              break;

            case 'mean':
              value = dataArray.reduce(function(acc, d) { return acc + d.value; }, 0) / dataArray.length;
              break;

            case 'distinct_count':
              var uniqueValues = {};
              dataArray.forEach(function(d) { uniqueValues[d.value] = true; });
              value = Object.keys(uniqueValues).length;
              break;

            case 'duplicates':
              var counts = {};
              dataArray.forEach(function(d) {
                counts[d.value] = (counts[d.value] || 0) + 1;
              });
              value = Object.values(counts).filter(function(count) { return count > 1; }).length;
              break;

            case 'min':
              value = Math.min.apply(null, dataArray.map(function(d) { return d.value; }));
              break;

            case 'max':
              value = Math.max.apply(null, dataArray.map(function(d) { return d.value; }));
              break;

            default:
              console.error('Invalid statistic specified:', x.settings.statistic);
              return;
          }

          // Function to format numbers with thousand separators
          function numberWithSep(x, bigMark) {
            if (typeof x !== "string") {
              x = x.toString();
            }
            var parts = x.split(".");
            parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, bigMark);
            return parts.join(".");
          }

          // Apply formatting
          if (x.settings.digits !== null) {
            value = parseFloat(value).toFixed(x.settings.digits);
          }
          if (x.settings.big_mark) {
            value = numberWithSep(value, x.settings.big_mark);
          }

          // Apply prefix and suffix
          var displayValue = value;

          // Always apply prefix if provided
          if (x.settings.prefix) {
            displayValue = x.settings.prefix + displayValue;
          }

          // For rate statistics, append '%' and skip suffix
          if (x.settings.statistic === 'count_rate' || x.settings.statistic === 'numeric_rate') {
            displayValue = displayValue + '%';
            // Do not apply suffix
          } else {
            // Apply suffix if provided
            if (x.settings.suffix) {
              displayValue = displayValue + x.settings.suffix;
            }
          }

          // Update the element's text content
          el.innerText = displayValue;
        };

        // Set up Crosstalk filter and selection handles
        var ct_filter = new crosstalk.FilterHandle();
        ct_filter.setGroup(x.settings.crosstalk_group);
        ct_filter.on("change", function(e) {
          if (e.value) {
            update(filterKeys(data, e.value));
          } else {
            update(data);
          }
        });

        var ct_sel = new crosstalk.SelectionHandle();
        ct_sel.setGroup(x.settings.crosstalk_group);
        ct_sel.on("change", function(e) {
          if (e.value && e.value.length) {
            update(filterKeys(data, e.value));
          } else {
            update(data);
          }
        });

        // Initial update
        update(data);
      },

      resize: function(width, height) {
        // Code to handle resizing if necessary
        // Currently not needed as the widget displays a single value
      }

    };
  }
});
