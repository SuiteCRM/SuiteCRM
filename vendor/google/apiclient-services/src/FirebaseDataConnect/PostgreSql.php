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

namespace Google\Service\FirebaseDataConnect;

class PostgreSql extends \Google\Model
{
  protected $cloudSqlType = CloudSqlInstance::class;
  protected $cloudSqlDataType = '';
  /**
   * @var string
   */
  public $database;
  /**
   * @var string
   */
  public $schemaMigration;
  /**
   * @var string
   */
  public $schemaValidation;
  /**
   * @var bool
   */
  public $unlinked;

  /**
   * @param CloudSqlInstance
   */
  public function setCloudSql(CloudSqlInstance $cloudSql)
  {
    $this->cloudSql = $cloudSql;
  }
  /**
   * @return CloudSqlInstance
   */
  public function getCloudSql()
  {
    return $this->cloudSql;
  }
  /**
   * @param string
   */
  public function setDatabase($database)
  {
    $this->database = $database;
  }
  /**
   * @return string
   */
  public function getDatabase()
  {
    return $this->database;
  }
  /**
   * @param string
   */
  public function setSchemaMigration($schemaMigration)
  {
    $this->schemaMigration = $schemaMigration;
  }
  /**
   * @return string
   */
  public function getSchemaMigration()
  {
    return $this->schemaMigration;
  }
  /**
   * @param string
   */
  public function setSchemaValidation($schemaValidation)
  {
    $this->schemaValidation = $schemaValidation;
  }
  /**
   * @return string
   */
  public function getSchemaValidation()
  {
    return $this->schemaValidation;
  }
  /**
   * @param bool
   */
  public function setUnlinked($unlinked)
  {
    $this->unlinked = $unlinked;
  }
  /**
   * @return bool
   */
  public function getUnlinked()
  {
    return $this->unlinked;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(PostgreSql::class, 'Google_Service_FirebaseDataConnect_PostgreSql');
