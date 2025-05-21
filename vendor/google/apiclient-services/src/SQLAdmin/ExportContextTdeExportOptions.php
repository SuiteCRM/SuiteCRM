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

namespace Google\Service\SQLAdmin;

class ExportContextTdeExportOptions extends \Google\Model
{
  /**
   * @var string
   */
  public $certificatePath;
  /**
   * @var string
   */
  public $name;
  /**
   * @var string
   */
  public $privateKeyPassword;
  /**
   * @var string
   */
  public $privateKeyPath;

  /**
   * @param string
   */
  public function setCertificatePath($certificatePath)
  {
    $this->certificatePath = $certificatePath;
  }
  /**
   * @return string
   */
  public function getCertificatePath()
  {
    return $this->certificatePath;
  }
  /**
   * @param string
   */
  public function setName($name)
  {
    $this->name = $name;
  }
  /**
   * @return string
   */
  public function getName()
  {
    return $this->name;
  }
  /**
   * @param string
   */
  public function setPrivateKeyPassword($privateKeyPassword)
  {
    $this->privateKeyPassword = $privateKeyPassword;
  }
  /**
   * @return string
   */
  public function getPrivateKeyPassword()
  {
    return $this->privateKeyPassword;
  }
  /**
   * @param string
   */
  public function setPrivateKeyPath($privateKeyPath)
  {
    $this->privateKeyPath = $privateKeyPath;
  }
  /**
   * @return string
   */
  public function getPrivateKeyPath()
  {
    return $this->privateKeyPath;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ExportContextTdeExportOptions::class, 'Google_Service_SQLAdmin_ExportContextTdeExportOptions');
