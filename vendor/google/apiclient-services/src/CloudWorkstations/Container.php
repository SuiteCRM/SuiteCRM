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

namespace Google\Service\CloudWorkstations;

class Container extends \Google\Collection
{
  protected $collection_key = 'command';
  /**
   * @var string[]
   */
  public $args;
  /**
   * @var string[]
   */
  public $command;
  /**
   * @var string[]
   */
  public $env;
  /**
   * @var string
   */
  public $image;
  /**
   * @var int
   */
  public $runAsUser;
  /**
   * @var string
   */
  public $workingDir;

  /**
   * @param string[]
   */
  public function setArgs($args)
  {
    $this->args = $args;
  }
  /**
   * @return string[]
   */
  public function getArgs()
  {
    return $this->args;
  }
  /**
   * @param string[]
   */
  public function setCommand($command)
  {
    $this->command = $command;
  }
  /**
   * @return string[]
   */
  public function getCommand()
  {
    return $this->command;
  }
  /**
   * @param string[]
   */
  public function setEnv($env)
  {
    $this->env = $env;
  }
  /**
   * @return string[]
   */
  public function getEnv()
  {
    return $this->env;
  }
  /**
   * @param string
   */
  public function setImage($image)
  {
    $this->image = $image;
  }
  /**
   * @return string
   */
  public function getImage()
  {
    return $this->image;
  }
  /**
   * @param int
   */
  public function setRunAsUser($runAsUser)
  {
    $this->runAsUser = $runAsUser;
  }
  /**
   * @return int
   */
  public function getRunAsUser()
  {
    return $this->runAsUser;
  }
  /**
   * @param string
   */
  public function setWorkingDir($workingDir)
  {
    $this->workingDir = $workingDir;
  }
  /**
   * @return string
   */
  public function getWorkingDir()
  {
    return $this->workingDir;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(Container::class, 'Google_Service_CloudWorkstations_Container');
