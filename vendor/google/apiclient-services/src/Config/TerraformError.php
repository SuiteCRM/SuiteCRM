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

namespace Google\Service\Config;

class TerraformError extends \Google\Model
{
  protected $errorType = Status::class;
  protected $errorDataType = '';
  /**
   * @var string
   */
  public $errorDescription;
  /**
   * @var int
   */
  public $httpResponseCode;
  /**
   * @var string
   */
  public $resourceAddress;

  /**
   * @param Status
   */
  public function setError(Status $error)
  {
    $this->error = $error;
  }
  /**
   * @return Status
   */
  public function getError()
  {
    return $this->error;
  }
  /**
   * @param string
   */
  public function setErrorDescription($errorDescription)
  {
    $this->errorDescription = $errorDescription;
  }
  /**
   * @return string
   */
  public function getErrorDescription()
  {
    return $this->errorDescription;
  }
  /**
   * @param int
   */
  public function setHttpResponseCode($httpResponseCode)
  {
    $this->httpResponseCode = $httpResponseCode;
  }
  /**
   * @return int
   */
  public function getHttpResponseCode()
  {
    return $this->httpResponseCode;
  }
  /**
   * @param string
   */
  public function setResourceAddress($resourceAddress)
  {
    $this->resourceAddress = $resourceAddress;
  }
  /**
   * @return string
   */
  public function getResourceAddress()
  {
    return $this->resourceAddress;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(TerraformError::class, 'Google_Service_Config_TerraformError');
