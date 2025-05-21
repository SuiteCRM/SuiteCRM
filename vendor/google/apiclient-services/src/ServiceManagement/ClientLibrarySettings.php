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

namespace Google\Service\ServiceManagement;

class ClientLibrarySettings extends \Google\Model
{
  protected $cppSettingsType = CppSettings::class;
  protected $cppSettingsDataType = '';
  protected $dotnetSettingsType = DotnetSettings::class;
  protected $dotnetSettingsDataType = '';
  protected $goSettingsType = GoSettings::class;
  protected $goSettingsDataType = '';
  protected $javaSettingsType = JavaSettings::class;
  protected $javaSettingsDataType = '';
  /**
   * @var string
   */
  public $launchStage;
  protected $nodeSettingsType = NodeSettings::class;
  protected $nodeSettingsDataType = '';
  protected $phpSettingsType = PhpSettings::class;
  protected $phpSettingsDataType = '';
  protected $pythonSettingsType = PythonSettings::class;
  protected $pythonSettingsDataType = '';
  /**
   * @var bool
   */
  public $restNumericEnums;
  protected $rubySettingsType = RubySettings::class;
  protected $rubySettingsDataType = '';
  /**
   * @var string
   */
  public $version;

  /**
   * @param CppSettings
   */
  public function setCppSettings(CppSettings $cppSettings)
  {
    $this->cppSettings = $cppSettings;
  }
  /**
   * @return CppSettings
   */
  public function getCppSettings()
  {
    return $this->cppSettings;
  }
  /**
   * @param DotnetSettings
   */
  public function setDotnetSettings(DotnetSettings $dotnetSettings)
  {
    $this->dotnetSettings = $dotnetSettings;
  }
  /**
   * @return DotnetSettings
   */
  public function getDotnetSettings()
  {
    return $this->dotnetSettings;
  }
  /**
   * @param GoSettings
   */
  public function setGoSettings(GoSettings $goSettings)
  {
    $this->goSettings = $goSettings;
  }
  /**
   * @return GoSettings
   */
  public function getGoSettings()
  {
    return $this->goSettings;
  }
  /**
   * @param JavaSettings
   */
  public function setJavaSettings(JavaSettings $javaSettings)
  {
    $this->javaSettings = $javaSettings;
  }
  /**
   * @return JavaSettings
   */
  public function getJavaSettings()
  {
    return $this->javaSettings;
  }
  /**
   * @param string
   */
  public function setLaunchStage($launchStage)
  {
    $this->launchStage = $launchStage;
  }
  /**
   * @return string
   */
  public function getLaunchStage()
  {
    return $this->launchStage;
  }
  /**
   * @param NodeSettings
   */
  public function setNodeSettings(NodeSettings $nodeSettings)
  {
    $this->nodeSettings = $nodeSettings;
  }
  /**
   * @return NodeSettings
   */
  public function getNodeSettings()
  {
    return $this->nodeSettings;
  }
  /**
   * @param PhpSettings
   */
  public function setPhpSettings(PhpSettings $phpSettings)
  {
    $this->phpSettings = $phpSettings;
  }
  /**
   * @return PhpSettings
   */
  public function getPhpSettings()
  {
    return $this->phpSettings;
  }
  /**
   * @param PythonSettings
   */
  public function setPythonSettings(PythonSettings $pythonSettings)
  {
    $this->pythonSettings = $pythonSettings;
  }
  /**
   * @return PythonSettings
   */
  public function getPythonSettings()
  {
    return $this->pythonSettings;
  }
  /**
   * @param bool
   */
  public function setRestNumericEnums($restNumericEnums)
  {
    $this->restNumericEnums = $restNumericEnums;
  }
  /**
   * @return bool
   */
  public function getRestNumericEnums()
  {
    return $this->restNumericEnums;
  }
  /**
   * @param RubySettings
   */
  public function setRubySettings(RubySettings $rubySettings)
  {
    $this->rubySettings = $rubySettings;
  }
  /**
   * @return RubySettings
   */
  public function getRubySettings()
  {
    return $this->rubySettings;
  }
  /**
   * @param string
   */
  public function setVersion($version)
  {
    $this->version = $version;
  }
  /**
   * @return string
   */
  public function getVersion()
  {
    return $this->version;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ClientLibrarySettings::class, 'Google_Service_ServiceManagement_ClientLibrarySettings');
