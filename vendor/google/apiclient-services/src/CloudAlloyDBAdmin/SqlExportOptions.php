<?php
/*
 * Copyright 2014 Google Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

namespace Google\Service\CloudAlloyDBAdmin;

class SqlExportOptions extends \Google\Collection
{
  protected $collection_key = 'tables';
  /**
   * @var bool
   */
  public $cleanTargetObjects;
  /**
   * @var bool
   */
  public $ifExistTargetObjects;
  /**
   * @var bool
   */
  public $schemaOnly;
  /**
   * @var string[]
   */
  public $tables;

  /**
   * @param bool
   */
  public function setCleanTargetObjects($cleanTargetObjects)
  {
    $this->cleanTargetObjects = $cleanTargetObjects;
  }
  /**
   * @return bool
   */
  public function getCleanTargetObjects()
  {
    return $this->cleanTargetObjects;
  }
  /**
   * @param bool
   */
  public function setIfExistTargetObjects($ifExistTargetObjects)
  {
    $this->ifExistTargetObjects = $ifExistTargetObjects;
  }
  /**
   * @return bool
   */
  public function getIfExistTargetObjects()
  {
    return $this->ifExistTargetObjects;
  }
  /**
   * @param bool
   */
  public function setSchemaOnly($schemaOnly)
  {
    $this->schemaOnly = $schemaOnly;
  }
  /**
   * @return bool
   */
  public function getSchemaOnly()
  {
    return $this->schemaOnly;
  }
  /**
   * @param string[]
   */
  public function setTables($tables)
  {
    $this->tables = $tables;
  }
  /**
   * @return string[]
   */
  public function getTables()
  {
    return $this->tables;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(SqlExportOptions::class, 'Google_Service_CloudAlloyDBAdmin_SqlExportOptions');
