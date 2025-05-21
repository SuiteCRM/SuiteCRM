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

namespace Google\Service\ContainerAnalysis;

class ContaineranalysisGoogleDevtoolsCloudbuildV1Artifacts extends \Google\Collection
{
  protected $collection_key = 'pythonPackages';
  protected $goModulesType = ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsGoModule::class;
  protected $goModulesDataType = 'array';
  /**
   * @var string[]
   */
  public $images;
  protected $mavenArtifactsType = ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsMavenArtifact::class;
  protected $mavenArtifactsDataType = 'array';
  protected $npmPackagesType = ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsNpmPackage::class;
  protected $npmPackagesDataType = 'array';
  protected $objectsType = ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsArtifactObjects::class;
  protected $objectsDataType = '';
  protected $pythonPackagesType = ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsPythonPackage::class;
  protected $pythonPackagesDataType = 'array';

  /**
   * @param ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsGoModule[]
   */
  public function setGoModules($goModules)
  {
    $this->goModules = $goModules;
  }
  /**
   * @return ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsGoModule[]
   */
  public function getGoModules()
  {
    return $this->goModules;
  }
  /**
   * @param string[]
   */
  public function setImages($images)
  {
    $this->images = $images;
  }
  /**
   * @return string[]
   */
  public function getImages()
  {
    return $this->images;
  }
  /**
   * @param ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsMavenArtifact[]
   */
  public function setMavenArtifacts($mavenArtifacts)
  {
    $this->mavenArtifacts = $mavenArtifacts;
  }
  /**
   * @return ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsMavenArtifact[]
   */
  public function getMavenArtifacts()
  {
    return $this->mavenArtifacts;
  }
  /**
   * @param ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsNpmPackage[]
   */
  public function setNpmPackages($npmPackages)
  {
    $this->npmPackages = $npmPackages;
  }
  /**
   * @return ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsNpmPackage[]
   */
  public function getNpmPackages()
  {
    return $this->npmPackages;
  }
  /**
   * @param ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsArtifactObjects
   */
  public function setObjects(ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsArtifactObjects $objects)
  {
    $this->objects = $objects;
  }
  /**
   * @return ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsArtifactObjects
   */
  public function getObjects()
  {
    return $this->objects;
  }
  /**
   * @param ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsPythonPackage[]
   */
  public function setPythonPackages($pythonPackages)
  {
    $this->pythonPackages = $pythonPackages;
  }
  /**
   * @return ContaineranalysisGoogleDevtoolsCloudbuildV1ArtifactsPythonPackage[]
   */
  public function getPythonPackages()
  {
    return $this->pythonPackages;
  }
}

// Adding a class alias for backwards compatibility with the previous class name.
class_alias(ContaineranalysisGoogleDevtoolsCloudbuildV1Artifacts::class, 'Google_Service_ContainerAnalysis_ContaineranalysisGoogleDevtoolsCloudbuildV1Artifacts');
