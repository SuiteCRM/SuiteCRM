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

namespace Google\Service\CloudDataplex;

class GoogleCloudDataplexV1MetadataJobImportJobSpec extends \Google\Model
{
  /**
   * @var string
   */
  public $aspectSyncMode;
  /**
   * @var string
   */
  public $entrySyncMode;
  /**
   * @var string
   */
  public $logLevel;
  protected $scopeType = GoogleCloudDataplexV1MetadataJobImportJobSpecImportJobScope::class;
  protected $scopeDataType = '';
  /**
   * @var string
   */
  public $sourceCreateTime;
  /**
   * @var string
   */
  public $sourceStorageUri;

  /**
   * @param string
   */
  public function setAspectSyncMode($aspectSyncMode)
  {
    $this->aspectSyncMode = $aspectSyncMode;
  }
  /**
   * @return string
   */
  public function getAspectSyncMode()
  {
    return $this->aspectSyncMode;
  }
  /**
   * @param string
   */
  public function setEntrySyncMode($entrySyncMode)
  {
    $this->entrySyncMode = $entrySyncMode;
  }
  /**
   * @return string
   */
  public function getEntrySyncMode()
  {
    return $this->entrySyncMode;
  }
  /**
   * @param string
   */
  public function setLogLevel($logLevel)
  {
    $this->logLevel = $logLevel;
  }
  /**
   * @return string
   */
  public function getLogLevel()
  {
    return $this->logLevel;
  }
  /**
   * @param GoogleCloudDataplexV1MetadataJobImportJobSpecImportJobScope
   */
  public function setScope(GoogleCloudDataplexV1MetadataJobImportJobSpecImportJobScope $scope)
  {
    $this->scope = $scope;
  }
  /**
   * @return GoogleCloudDataplexV1MetadataJobImportJobSpecImportJobScope
   */
  public function getScope()
  {
    return $this->scope;
  }
  /**
   * @param string
   */
  public function setSourceCreateTime($sourceCreateTime)
  {
    $this->sourceCreateTime = $sourceCreateTime;
  }
  /**
   * @return string
   */
  public function getSourceCreateTime()
  {
    return $this->sourceCreateTime;
  }
  /**
   * @param string
   */
  public function setSourceStorageUri($sourceStorageUri)
  {
    $this->sourceStorageUri = $sourceStorageUri;
  }
  /**
   * @return string
   */
  public function getSourceStorageUri()
  {
    return $this->sourceStorageUri;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudDataplexV1MetadataJobImportJobSpec::class, 'Google_Service_CloudDataplex_GoogleCloudDataplexV1MetadataJobImportJobSpec');
