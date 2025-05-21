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

namespace Google\Service\NetAppFiles;

class VerifyKmsConfigResponse extends \Google\Model
{
  /**
   * @var string
   */
  public $healthError;
  /**
   * @var bool
   */
  public $healthy;
  /**
   * @var string
   */
  public $instructions;

  /**
   * @param string
   */
  public function setHealthError($healthError)
  {
    $this->healthError = $healthError;
  }
  /**
   * @return string
   */
  public function getHealthError()
  {
    return $this->healthError;
  }
  /**
   * @param bool
   */
  public function setHealthy($healthy)
  {
    $this->healthy = $healthy;
  }
  /**
   * @return bool
   */
  public function getHealthy()
  {
    return $this->healthy;
  }
  /**
   * @param string
   */
  public function setInstructions($instructions)
  {
    $this->instructions = $instructions;
  }
  /**
   * @return string
   */
  public function getInstructions()
  {
    return $this->instructions;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(VerifyKmsConfigResponse::class, 'Google_Service_NetAppFiles_VerifyKmsConfigResponse');
