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

namespace Google\Service\FirebaseHosting;

class HttpUpdate extends \Google\Model
{
  protected $checkErrorType = Status::class;
  protected $checkErrorDataType = '';
  /**
   * @var string
   */
  public $desired;
  /**
   * @var string
   */
  public $discovered;
  /**
   * @var string
   */
  public $lastCheckTime;
  /**
   * @var string
   */
  public $path;

  /**
   * @param Status
   */
  public function setCheckError(Status $checkError)
  {
    $this->checkError = $checkError;
  }
  /**
   * @return Status
   */
  public function getCheckError()
  {
    return $this->checkError;
  }
  /**
   * @param string
   */
  public function setDesired($desired)
  {
    $this->desired = $desired;
  }
  /**
   * @return string
   */
  public function getDesired()
  {
    return $this->desired;
  }
  /**
   * @param string
   */
  public function setDiscovered($discovered)
  {
    $this->discovered = $discovered;
  }
  /**
   * @return string
   */
  public function getDiscovered()
  {
    return $this->discovered;
  }
  /**
   * @param string
   */
  public function setLastCheckTime($lastCheckTime)
  {
    $this->lastCheckTime = $lastCheckTime;
  }
  /**
   * @return string
   */
  public function getLastCheckTime()
  {
    return $this->lastCheckTime;
  }
  /**
   * @param string
   */
  public function setPath($path)
  {
    $this->path = $path;
  }
  /**
   * @return string
   */
  public function getPath()
  {
    return $this->path;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(HttpUpdate::class, 'Google_Service_FirebaseHosting_HttpUpdate');
