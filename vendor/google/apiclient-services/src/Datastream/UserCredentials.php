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

namespace Google\Service\Datastream;

class UserCredentials extends \Google\Model
{
  /**
   * @var string
   */
  public $password;
  /**
   * @var string
   */
  public $secretManagerStoredPassword;
  /**
   * @var string
   */
  public $secretManagerStoredSecurityToken;
  /**
   * @var string
   */
  public $securityToken;
  /**
   * @var string
   */
  public $username;

  /**
   * @param string
   */
  public function setPassword($password)
  {
    $this->password = $password;
  }
  /**
   * @return string
   */
  public function getPassword()
  {
    return $this->password;
  }
  /**
   * @param string
   */
  public function setSecretManagerStoredPassword($secretManagerStoredPassword)
  {
    $this->secretManagerStoredPassword = $secretManagerStoredPassword;
  }
  /**
   * @return string
   */
  public function getSecretManagerStoredPassword()
  {
    return $this->secretManagerStoredPassword;
  }
  /**
   * @param string
   */
  public function setSecretManagerStoredSecurityToken($secretManagerStoredSecurityToken)
  {
    $this->secretManagerStoredSecurityToken = $secretManagerStoredSecurityToken;
  }
  /**
   * @return string
   */
  public function getSecretManagerStoredSecurityToken()
  {
    return $this->secretManagerStoredSecurityToken;
  }
  /**
   * @param string
   */
  public function setSecurityToken($securityToken)
  {
    $this->securityToken = $securityToken;
  }
  /**
   * @return string
   */
  public function getSecurityToken()
  {
    return $this->securityToken;
  }
  /**
   * @param string
   */
  public function setUsername($username)
  {
    $this->username = $username;
  }
  /**
   * @return string
   */
  public function getUsername()
  {
    return $this->username;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(UserCredentials::class, 'Google_Service_Datastream_UserCredentials');
