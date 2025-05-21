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

namespace Google\Service\Games;

class LinkPersonaRequest extends \Google\Model
{
  /**
   * @var string
   */
  public $cardinalityConstraint;
  /**
   * @var string
   */
  public $conflictingLinksResolutionPolicy;
  /**
   * @var string
   */
  public $expireTime;
  /**
   * @var string
   */
  public $persona;
  /**
   * @var string
   */
  public $sessionId;
  /**
   * @var string
   */
  public $token;
  /**
   * @var string
   */
  public $ttl;

  /**
   * @param string
   */
  public function setCardinalityConstraint($cardinalityConstraint)
  {
    $this->cardinalityConstraint = $cardinalityConstraint;
  }
  /**
   * @return string
   */
  public function getCardinalityConstraint()
  {
    return $this->cardinalityConstraint;
  }
  /**
   * @param string
   */
  public function setConflictingLinksResolutionPolicy($conflictingLinksResolutionPolicy)
  {
    $this->conflictingLinksResolutionPolicy = $conflictingLinksResolutionPolicy;
  }
  /**
   * @return string
   */
  public function getConflictingLinksResolutionPolicy()
  {
    return $this->conflictingLinksResolutionPolicy;
  }
  /**
   * @param string
   */
  public function setExpireTime($expireTime)
  {
    $this->expireTime = $expireTime;
  }
  /**
   * @return string
   */
  public function getExpireTime()
  {
    return $this->expireTime;
  }
  /**
   * @param string
   */
  public function setPersona($persona)
  {
    $this->persona = $persona;
  }
  /**
   * @return string
   */
  public function getPersona()
  {
    return $this->persona;
  }
  /**
   * @param string
   */
  public function setSessionId($sessionId)
  {
    $this->sessionId = $sessionId;
  }
  /**
   * @return string
   */
  public function getSessionId()
  {
    return $this->sessionId;
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
  /**
   * @param string
   */
  public function setTtl($ttl)
  {
    $this->ttl = $ttl;
  }
  /**
   * @return string
   */
  public function getTtl()
  {
    return $this->ttl;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(LinkPersonaRequest::class, 'Google_Service_Games_LinkPersonaRequest');
