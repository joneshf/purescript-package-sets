'use strict';

exports.close_ = function(unit, db) {
  db.close();
  return unit;
};

exports.exec_ = function(sql, db) {
  return db.exec(sql);
};

exports.new_ = function(xs) {
  return new SQL.Database(xs);
};
