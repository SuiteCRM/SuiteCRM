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

namespace Google\Service\FirebaseDataConnect;

class Impersonation extends \Google\Model
{
  /**
   * @var array[]
   */
  public $authClaims;
  /**
   * @var bool
   */
  public $unauthenticated;

  /**
   * @param array[]
   */
  public function setAuthClaims($authClaims)
  {
    $this->authClaims = $authClaims;
  }
  /**
   * @return array[]
   */
  public function getAuthClaims()
  {
    return $this->authClaims;
  }
  /**
   * @param bool
   */
  public function setUnauthenticated($unauthenticated)
  {
    $this->unauthenticated = $unauthenticated;
  }
  /**
   * @return bool
   */
  public function getUnauthenticated()
  {
    return $this->unauthenticated;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Impersonation::class, 'Google_Service_FirebaseDataConnect_Impersonation');
