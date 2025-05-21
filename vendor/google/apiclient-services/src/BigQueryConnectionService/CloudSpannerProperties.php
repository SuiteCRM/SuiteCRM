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

namespace Google\Service\BigQueryConnectionService;

class CloudSpannerProperties extends \Google\Model
{
  /**
   * @var string
   */
  public $database;
  /**
   * @var string
   */
  public $databaseRole;
  /**
   * @var int
   */
  public $maxParallelism;
  /**
   * @var bool
   */
  public $useDataBoost;
  /**
   * @var bool
   */
  public $useParallelism;
  /**
   * @var bool
   */
  public $useServerlessAnalytics;

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
  public function setDatabaseRole($databaseRole)
  {
    $this->databaseRole = $databaseRole;
  }
  /**
   * @return string
   */
  public function getDatabaseRole()
  {
    return $this->databaseRole;
  }
  /**
   * @param int
   */
  public function setMaxParallelism($maxParallelism)
  {
    $this->maxParallelism = $maxParallelism;
  }
  /**
   * @return int
   */
  public function getMaxParallelism()
  {
    return $this->maxParallelism;
  }
  /**
   * @param bool
   */
  public function setUseDataBoost($useDataBoost)
  {
    $this->useDataBoost = $useDataBoost;
  }
  /**
   * @return bool
   */
  public function getUseDataBoost()
  {
    return $this->useDataBoost;
  }
  /**
   * @param bool
   */
  public function setUseParallelism($useParallelism)
  {
    $this->useParallelism = $useParallelism;
  }
  /**
   * @return bool
   */
  public function getUseParallelism()
  {
    return $this->useParallelism;
  }
  /**
   * @param bool
   */
  public function setUseServerlessAnalytics($useServerlessAnalytics)
  {
    $this->useServerlessAnalytics = $useServerlessAnalytics;
  }
  /**
   * @return bool
   */
  public function getUseServerlessAnalytics()
  {
    return $this->useServerlessAnalytics;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(CloudSpannerProperties::class, 'Google_Service_BigQueryConnectionService_CloudSpannerProperties');
