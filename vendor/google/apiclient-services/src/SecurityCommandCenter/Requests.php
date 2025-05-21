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

namespace Google\Service\SecurityCommandCenter;

class Requests extends \Google\Model
{
  /**
   * @var int
   */
  public $longTermAllowed;
  /**
   * @var int
   */
  public $longTermDenied;
  public $ratio;
  /**
   * @var int
   */
  public $shortTermAllowed;

  /**
   * @param int
   */
  public function setLongTermAllowed($longTermAllowed)
  {
    $this->longTermAllowed = $longTermAllowed;
  }
  /**
   * @return int
   */
  public function getLongTermAllowed()
  {
    return $this->longTermAllowed;
  }
  /**
   * @param int
   */
  public function setLongTermDenied($longTermDenied)
  {
    $this->longTermDenied = $longTermDenied;
  }
  /**
   * @return int
   */
  public function getLongTermDenied()
  {
    return $this->longTermDenied;
  }
  public function setRatio($ratio)
  {
    $this->ratio = $ratio;
  }
  public function getRatio()
  {
    return $this->ratio;
  }
  /**
   * @param int
   */
  public function setShortTermAllowed($shortTermAllowed)
  {
    $this->shortTermAllowed = $shortTermAllowed;
  }
  /**
   * @return int
   */
  public function getShortTermAllowed()
  {
    return $this->shortTermAllowed;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Requests::class, 'Google_Service_SecurityCommandCenter_Requests');
