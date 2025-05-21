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

namespace Google\Service\Batch;

class LogsPolicy extends \Google\Model
{
  protected $cloudLoggingOptionType = CloudLoggingOption::class;
  protected $cloudLoggingOptionDataType = '';
  /**
   * @var string
   */
  public $destination;
  /**
   * @var string
   */
  public $logsPath;

  /**
   * @param CloudLoggingOption
   */
  public function setCloudLoggingOption(CloudLoggingOption $cloudLoggingOption)
  {
    $this->cloudLoggingOption = $cloudLoggingOption;
  }
  /**
   * @return CloudLoggingOption
   */
  public function getCloudLoggingOption()
  {
    return $this->cloudLoggingOption;
  }
  /**
   * @param string
   */
  public function setDestination($destination)
  {
    $this->destination = $destination;
  }
  /**
   * @return string
   */
  public function getDestination()
  {
    return $this->destination;
  }
  /**
   * @param string
   */
  public function setLogsPath($logsPath)
  {
    $this->logsPath = $logsPath;
  }
  /**
   * @return string
   */
  public function getLogsPath()
  {
    return $this->logsPath;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LogsPolicy::class, 'Google_Service_Batch_LogsPolicy');
