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

namespace Google\Service\AndroidEnterprise;

class EnrollmentToken extends \Google\Model
{
  /**
   * @var string
   */
  public $duration;
  /**
   * @var string
   */
  public $enrollmentTokenType;
  /**
   * @var string
   */
  public $token;

  /**
   * @param string
   */
  public function setDuration($duration)
  {
    $this->duration = $duration;
  }
  /**
   * @return string
   */
  public function getDuration()
  {
    return $this->duration;
  }
  /**
   * @param string
   */
  public function setEnrollmentTokenType($enrollmentTokenType)
  {
    $this->enrollmentTokenType = $enrollmentTokenType;
  }
  /**
   * @return string
   */
  public function getEnrollmentTokenType()
  {
    return $this->enrollmentTokenType;
  }
  /**
   * @param string
   */
  public function setToken($token)
  {
    $this->token = $token;
  }
  /**
   * @return string
   */
  public function getToken()
  {
    return $this->token;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(EnrollmentToken::class, 'Google_Service_AndroidEnterprise_EnrollmentToken');
