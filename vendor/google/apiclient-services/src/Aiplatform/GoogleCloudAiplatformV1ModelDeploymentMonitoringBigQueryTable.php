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

namespace Google\Service\Aiplatform;

class GoogleCloudAiplatformV1ModelDeploymentMonitoringBigQueryTable extends \Google\Model
{
  /**
   * @var string
   */
  public $bigqueryTablePath;
  /**
   * @var string
   */
  public $logSource;
  /**
   * @var string
   */
  public $logType;
  /**
   * @var string
   */
  public $requestResponseLoggingSchemaVersion;

  /**
   * @param string
   */
  public function setBigqueryTablePath($bigqueryTablePath)
  {
    $this->bigqueryTablePath = $bigqueryTablePath;
  }
  /**
   * @return string
   */
  public function getBigqueryTablePath()
  {
    return $this->bigqueryTablePath;
  }
  /**
   * @param string
   */
  public function setLogSource($logSource)
  {
    $this->logSource = $logSource;
  }
  /**
   * @return string
   */
  public function getLogSource()
  {
    return $this->logSource;
  }
  /**
   * @param string
   */
  public function setLogType($logType)
  {
    $this->logType = $logType;
  }
  /**
   * @return string
   */
  public function getLogType()
  {
    return $this->logType;
  }
  /**
   * @param string
   */
  public function setRequestResponseLoggingSchemaVersion($requestResponseLoggingSchemaVersion)
  {
    $this->requestResponseLoggingSchemaVersion = $requestResponseLoggingSchemaVersion;
  }
  /**
   * @return string
   */
  public function getRequestResponseLoggingSchemaVersion()
  {
    return $this->requestResponseLoggingSchemaVersion;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(GoogleCloudAiplatformV1ModelDeploymentMonitoringBigQueryTable::class, 'Google_Service_Aiplatform_GoogleCloudAiplatformV1ModelDeploymentMonitoringBigQueryTable');
