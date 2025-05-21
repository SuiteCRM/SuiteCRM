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

namespace Google\Service\DeploymentManager;

class ResourceUpdateErrorErrors extends \Google\Collection
{
  protected $collection_key = 'errorDetails';
  /**
   * @var string[]
   */
  public $arguments;
  /**
   * @var string
   */
  public $code;
  protected $debugInfoType = DebugInfo::class;
  protected $debugInfoDataType = '';
  protected $errorDetailsType = ResourceUpdateErrorErrorsErrorDetails::class;
  protected $errorDetailsDataType = 'array';
  /**
   * @var string
   */
  public $location;
  /**
   * @var string
   */
  public $message;

  /**
   * @param string[]
   */
  public function setArguments($arguments)
  {
    $this->arguments = $arguments;
  }
  /**
   * @return string[]
   */
  public function getArguments()
  {
    return $this->arguments;
  }
  /**
   * @param string
   */
  public function setCode($code)
  {
    $this->code = $code;
  }
  /**
   * @return string
   */
  public function getCode()
  {
    return $this->code;
  }
  /**
   * @param DebugInfo
   */
  public function setDebugInfo(DebugInfo $debugInfo)
  {
    $this->debugInfo = $debugInfo;
  }
  /**
   * @return DebugInfo
   */
  public function getDebugInfo()
  {
    return $this->debugInfo;
  }
  /**
   * @param ResourceUpdateErrorErrorsErrorDetails[]
   */
  public function setErrorDetails($errorDetails)
  {
    $this->errorDetails = $errorDetails;
  }
  /**
   * @return ResourceUpdateErrorErrorsErrorDetails[]
   */
  public function getErrorDetails()
  {
    return $this->errorDetails;
  }
  /**
   * @param string
   */
  public function setLocation($location)
  {
    $this->location = $location;
  }
  /**
   * @return string
   */
  public function getLocation()
  {
    return $this->location;
  }
  /**
   * @param string
   */
  public function setMessage($message)
  {
    $this->message = $message;
  }
  /**
   * @return string
   */
  public function getMessage()
  {
    return $this->message;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ResourceUpdateErrorErrors::class, 'Google_Service_DeploymentManager_ResourceUpdateErrorErrors');
