(function () {
  'use strict';
  function ready(fn) { document.readyState !== 'loading' ? fn() : document.addEventListener('DOMContentLoaded', fn); }
  function parse(value) { try { var v = JSON.parse(value || '[]'); return Array.isArray(v) ? v : []; } catch (e) { return []; } }
  function node(tag, text) { var n = document.createElement(tag); if (text) n.textContent = text; return n; }

  function enhance(fieldId, title, columns) {
    var field = document.getElementById(fieldId) || document.querySelector('[name="' + fieldId + '"]');
    if (!field || field.getAttribute('data-wit-enhanced') === '1') return;
    field.setAttribute('data-wit-enhanced', '1');
    field.style.display = 'none';
    var wrapper = node('div');
    wrapper.style.cssText = 'border:1px solid #ddd;padding:10px;margin:4px 0;background:#fbfbfb;';
    var heading = node('div', title);
    heading.style.fontWeight = 'bold';
    wrapper.appendChild(heading);
    var table = node('table');
    table.className = 'list view';
    table.style.width = '100%';
    var body = node('tbody');
    table.appendChild(body);
    wrapper.appendChild(table);
    var add = node('button', '+ Add ' + title.replace(/s$/, ''));
    add.type = 'button';
    add.className = 'button';
    wrapper.appendChild(add);
    field.parentNode.insertBefore(wrapper, field.nextSibling);

    function sync() {
      var output = [];
      Array.prototype.slice.call(body.querySelectorAll('tr')).forEach(function (tr) {
        var item = {};
        columns.forEach(function (column) {
          var input = tr.querySelector('[data-key="' + column.key + '"]');
          item[column.key] = input ? input.value.trim() : '';
        });
        if (Object.keys(item).some(function (key) { return item[key] !== ''; })) output.push(item);
      });
      field.value = JSON.stringify(output, null, 2);
    }

    function addRow(data) {
      var tr = node('tr');
      data = data || {};
      columns.forEach(function (column) {
        var td = node('td');
        var input = node('input');
        input.type = column.type || 'text';
        input.setAttribute('data-key', column.key);
        input.placeholder = column.label;
        input.value = data[column.key] || '';
        input.style.width = '95%';
        input.addEventListener('change', sync);
        input.addEventListener('keyup', sync);
        td.appendChild(input);
        tr.appendChild(td);
      });
      var td = node('td');
      var remove = node('button', 'Remove');
      remove.type = 'button';
      remove.className = 'button';
      remove.addEventListener('click', function () { body.removeChild(tr); sync(); });
      td.appendChild(remove);
      tr.appendChild(td);
      body.appendChild(tr);
      sync();
    }

    var rows = parse(field.value);
    if (!rows.length) rows = [{}];
    rows.forEach(addRow);
    add.addEventListener('click', function () { addRow({}); });
  }

  ready(function () {
    enhance('drivers_json_c', 'Drivers', [
      { key: 'name', label: 'Driver Name' },
      { key: 'dob', label: 'DOB', type: 'date' },
      { key: 'driver_license', label: 'DL #' },
      { key: 'license_state', label: 'DL State' },
      { key: 'relationship', label: 'Relationship' }
    ]);
    enhance('vehicles_json_c', 'Vehicles', [
      { key: 'vin', label: 'VIN' },
      { key: 'year', label: 'Year' },
      { key: 'make', label: 'Make' },
      { key: 'model', label: 'Model' },
      { key: 'body_class', label: 'Body' },
      { key: 'vehicle_type', label: 'Type' }
    ]);
    enhance('accidents_violations_json_c', 'Accidents / Violations', [
      { key: 'type', label: 'Type' },
      { key: 'date', label: 'Date', type: 'date' },
      { key: 'conviction_date', label: 'Conviction Date', type: 'date' },
      { key: 'description', label: 'Description' },
      { key: 'at_fault', label: 'At Fault?' }
    ]);
  });
}());
